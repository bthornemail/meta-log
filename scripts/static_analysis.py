#!/usr/bin/env python3
"""
Static Analysis Script for meta-log
Identifies unused functions, dead code, and optimization opportunities.
"""

import os
import re
import json
from collections import defaultdict
from pathlib import Path

class StaticAnalyzer:
    def __init__(self, root_dir):
        self.root_dir = Path(root_dir)
        self.function_defs = defaultdict(list)  # func_name -> [(file, line), ...]
        self.function_calls = defaultdict(int)  # func_name -> call_count
        self.requires = defaultdict(set)  # file -> set of required modules
        self.unused_functions = []
        self.unused_requires = []
        
    def find_elisp_functions(self):
        """Find all defun definitions in .el files."""
        for el_file in self.root_dir.rglob("*.el"):
            if "test" in str(el_file).lower() or "example" in str(el_file).lower():
                continue
                
            try:
                with open(el_file, 'r', encoding='utf-8', errors='ignore') as f:
                    for line_num, line in enumerate(f, 1):
                        # Match: (defun function-name
                        match = re.search(r'\(defun\s+([a-zA-Z0-9-]+)', line)
                        if match:
                            func_name = match.group(1)
                            rel_path = str(el_file.relative_to(self.root_dir))
                            self.function_defs[func_name].append((rel_path, line_num))
            except Exception as e:
                print(f"Error reading {el_file}: {e}")
    
    def find_scheme_functions(self):
        """Find all define definitions in .scm files."""
        for scm_file in self.root_dir.rglob("*.scm"):
            if "test" in str(scm_file).lower():
                continue
                
            try:
                with open(scm_file, 'r', encoding='utf-8', errors='ignore') as f:
                    for line_num, line in enumerate(f, 1):
                        # Match: (define (function-name
                        match = re.search(r'\(define\s+\(([a-zA-Z0-9-]+)', line)
                        if match:
                            func_name = match.group(1)
                            rel_path = str(scm_file.relative_to(self.root_dir))
                            self.function_defs[func_name].append((rel_path, line_num))
            except Exception as e:
                print(f"Error reading {scm_file}: {e}")
    
    def find_function_calls(self):
        """Find all function calls in codebase."""
        for code_file in self.root_dir.rglob("*.el"):
            if "test" in str(code_file).lower() or "example" in str(code_file).lower():
                continue
                
            try:
                with open(code_file, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()
                    # Find function calls: (function-name ...) or 'function-name or (apply 'function-name
                    for func_name in self.function_defs.keys():
                        # Pattern 1: (function-name ...)
                        pattern1 = rf'\(({re.escape(func_name)})\s'
                        # Pattern 2: 'function-name or "function-name
                        pattern2 = rf"['\"]({re.escape(func_name)})['\"]"
                        # Pattern 3: (apply 'function-name or (funcall 'function-name
                        pattern3 = rf"(apply|funcall)\s+['\"]({re.escape(func_name)})['\"]"
                        # Pattern 4: #'function-name (function reference)
                        pattern4 = rf"#\'({re.escape(func_name)})"
                        
                        matches = (len(re.findall(pattern1, content)) +
                                  len(re.findall(pattern2, content)) +
                                  len(re.findall(pattern3, content)) +
                                  len(re.findall(pattern4, content)))
                        if matches > 0:
                            self.function_calls[func_name] += matches
            except Exception as e:
                print(f"Error reading {code_file}: {e}")
        
        # Also check Scheme files
        for code_file in self.root_dir.rglob("*.scm"):
            if "test" in str(code_file).lower():
                continue
            try:
                with open(code_file, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()
                    for func_name in self.function_defs.keys():
                        pattern = rf'\(({re.escape(func_name)})\s'
                        matches = len(re.findall(pattern, content))
                        if matches > 0:
                            self.function_calls[func_name] += matches
            except Exception as e:
                print(f"Error reading {code_file}: {e}")
    
    def find_requires(self):
        """Find all require statements."""
        for el_file in self.root_dir.rglob("*.el"):
            if "test" in str(el_file).lower():
                continue
                
            try:
                with open(el_file, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()
                    # Match: (require 'module-name)
                    requires = re.findall(r"\(require\s+['\"]?([a-zA-Z0-9-]+)", content)
                    rel_path = str(el_file.relative_to(self.root_dir))
                    self.requires[rel_path] = set(requires)
            except Exception as e:
                print(f"Error reading {el_file}: {e}")
    
    def check_documented(self, func_name):
        """Check if function is documented in API-REFERENCE.md or README."""
        api_ref = self.root_dir / "docs" / "API-REFERENCE.md"
        readme = self.root_dir / "README.md"
        
        for doc_file in [api_ref, readme]:
            if doc_file.exists():
                try:
                    with open(doc_file, 'r', encoding='utf-8', errors='ignore') as f:
                        if func_name in f.read():
                            return True
                except:
                    pass
        return False
    
    def identify_unused_functions(self):
        """Identify functions that are defined but never called."""
        for func_name, locations in self.function_defs.items():
            # Skip internal/private functions (containing --)
            if '--' in func_name:
                continue
            # Skip test functions
            if func_name.startswith('test-') or 'test' in func_name.lower():
                continue
            # Skip demo functions
            if func_name.startswith('demo-'):
                continue
            # Skip example functions
            if 'example' in func_name.lower():
                continue
            
            # Check if function is called
            call_count = self.function_calls.get(func_name, 0)
            
            # Check if documented (public API functions are often documented but not called internally)
            is_documented = self.check_documented(func_name)
            
            # If defined but never called AND not documented
            if call_count == 0 and not is_documented:
                for file_path, line_num in locations:
                    # Skip if it's in demos or examples
                    if 'demo' in file_path.lower() or 'example' in file_path.lower():
                        continue
                    
                    # Check if it's a public API function (starts with meta-log-)
                    if func_name.startswith('meta-log-'):
                        self.unused_functions.append({
                            'name': func_name,
                            'file': file_path,
                            'line': line_num,
                            'type': 'public_api_undocumented'
                        })
                    else:
                        self.unused_functions.append({
                            'name': func_name,
                            'file': file_path,
                            'line': line_num,
                            'type': 'internal'
                        })
            # If documented but never called, it's likely a public API meant for users
            elif call_count == 0 and is_documented:
                # Still note it, but mark as public API
                for file_path, line_num in locations:
                    if func_name.startswith('meta-log-'):
                        self.unused_functions.append({
                            'name': func_name,
                            'file': file_path,
                            'line': line_num,
                            'type': 'public_api_documented',
                            'note': 'Documented public API - likely called by users, not internally'
                        })
    
    def analyze(self):
        """Run full analysis."""
        print("Finding function definitions...")
        self.find_elisp_functions()
        self.find_scheme_functions()
        print(f"Found {len(self.function_defs)} unique functions")
        
        print("Finding function calls...")
        self.find_function_calls()
        
        print("Finding requires...")
        self.find_requires()
        
        print("Identifying unused functions...")
        self.identify_unused_functions()
        
        print(f"Found {len(self.unused_functions)} potentially unused functions")
    
    def generate_report(self, output_file):
        """Generate markdown report."""
        with open(output_file, 'w') as f:
            f.write("# Static Analysis Report\n\n")
            f.write(f"**Generated**: {__import__('datetime').datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            
            f.write("## Summary\n\n")
            f.write(f"- **Total Functions Defined**: {len(self.function_defs)}\n")
            f.write(f"- **Functions Called**: {len(self.function_calls)}\n")
            f.write(f"- **Potentially Unused Functions**: {len(self.unused_functions)}\n\n")
            
            f.write("## Potentially Unused Functions\n\n")
            if self.unused_functions:
                f.write("| Function | File | Line | Type | Notes |\n")
                f.write("|----------|------|------|------|-------|\n")
                
                # Group by type
                public_api = [f for f in self.unused_functions if f['type'] == 'public_api']
                internal = [f for f in self.unused_functions if f['type'] == 'internal']
                
                for func in sorted(public_api + internal, key=lambda x: (x['type'], x['file'])):
                    notes = "⚠️ Review before removal" if func['type'] == 'public_api' else "Internal function"
                    f.write(f"| `{func['name']}` | `{func['file']}` | {func['line']} | {func['type']} | {notes} |\n")
            else:
                f.write("✅ No unused functions found.\n\n")
            
            f.write("\n## Analysis Notes\n\n")
            f.write("- Functions marked as 'internal' (containing `--`) are excluded\n")
            f.write("- Test functions are excluded\n")
            f.write("- Functions only called in their definition file may be legitimate\n")
            f.write("- **Manual review required** before removing any functions\n")
            f.write("- Public API functions should be reviewed especially carefully\n\n")
            
            f.write("## Recommendations\n\n")
            if self.unused_functions:
                f.write("1. **Review Public API Functions**: Check if unused public API functions should be:\n")
                f.write("   - Documented as optional/advanced features\n")
                f.write("   - Deprecated if no longer needed\n")
                f.write("   - Kept for backward compatibility\n\n")
                f.write("2. **Internal Functions**: Review if these are:\n")
                f.write("   - Helper functions for future use\n")
                f.write("   - Part of incomplete implementations\n")
                f.write("   - Truly unused and safe to remove\n\n")
            else:
                f.write("✅ No action needed - all functions appear to be in use.\n\n")

if __name__ == "__main__":
    analyzer = StaticAnalyzer("/home/main/meta-log")
    analyzer.analyze()
    analyzer.generate_report("/home/main/meta-log/docs/STATIC-ANALYSIS-REPORT.md")
    print(f"\nReport generated: docs/STATIC-ANALYSIS-REPORT.md")

