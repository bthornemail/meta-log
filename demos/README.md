# meta-log Demos

Interactive demonstrations of meta-log capabilities for different use cases.

## Available Demos

### 1. Personal Knowledge Base (`01-personal-knowledge-base.el`)
**Use Case:** Individual knowledge management

Build and query a personal knowledge base from your notes and documents.

**What it demonstrates:**
- Ingesting notes from Org files
- Building a knowledge graph automatically
- Semantic search across your knowledge
- Natural language queries
- Finding connections between concepts

**How to run:**
```elisp
(load-file "demos/01-personal-knowledge-base.el")
(demo-personal-kb-full)
```

**Best for:**
- Students organizing study materials
- Researchers managing literature
- Writers building world knowledge
- Anyone with personal notes to organize

---

### 2. Research Assistant (`02-research-assistant.el`)
**Use Case:** AI-powered research and learning

An intelligent assistant that learns from interactions and helps you explore topics.

**What it demonstrates:**
- Learning from Q&A interactions
- Maintaining research context
- Interactive chat interface
- Paper and citation management
- Knowledge synthesis across topics

**How to run:**
```elisp
(load-file "demos/02-research-assistant.el")
(demo-research-full)
```

**Best for:**
- Academic researchers
- PhD students
- Technical writers
- Continuous learners

---

### 3. Code Analysis (`03-code-analysis.el`)
**Use Case:** Understanding and documenting code

Analyze codebases, track dependencies, and generate documentation with AI.

**What it demonstrates:**
- Code structure analysis
- Dependency mapping
- Automatic documentation generation
- Refactoring insights
- Semantic code search

**How to run:**
```elisp
(load-file "demos/03-code-analysis.el")
(demo-code-full)
```

**Best for:**
- Software developers
- Code reviewers
- Technical leads
- Documentation writers

---

### 4. Team Collaboration (`04-team-collaboration.el`)
**Use Case:** Team knowledge sharing and collaboration

Federated knowledge sharing across team members with different expertise.

**What it demonstrates:**
- Peer-to-peer federation
- Team expertise discovery
- Multi-perspective learning
- Collaborative code review
- Team metrics and insights

**How to run:**
```elisp
(load-file "demos/04-team-collaboration.el")
(demo-collab-full)
```

**Best for:**
- Development teams
- Research groups
- Distributed teams
- Open source projects

---

## Quick Start

### Run All Demos
```elisp
;; Load meta-log
(require 'meta-log)

;; Run a specific demo
(load-file "demos/01-personal-knowledge-base.el")
(demo-personal-kb-full)
```

### From Command Line
```bash
# Run a demo in batch mode
emacs --batch -l demos/01-personal-knowledge-base.el -f demo-personal-kb-full

# Or interactively
emacs -l demos/01-personal-knowledge-base.el
```

---

## Demo Features Summary

| Feature | KB | Research | Code | Team |
|---------|----|---------:|------|------|
| Knowledge Graph | ✓ | ✓ | ✓ | ✓ |
| Natural Language | ✓ | ✓ | ✓ | ✓ |
| Learning/Adaptation | | ✓ | | ✓ |
| File Ingestion | ✓ | ✓ | ✓ | |
| Chat Interface | | ✓ | | |
| Federation | | | | ✓ |
| Code Analysis | | | ✓ | |
| Documentation | | | ✓ | |

---

## Customizing Demos

Each demo is self-contained and can be customized:

```elisp
;; Customize knowledge base location
(setq demo-kb-directory "~/my-notes")

;; Customize LLM provider
(setq meta-log-llm-provider 'anthropic)
(setq meta-log-llm-api-key "your-key")

;; Run demo with custom settings
(demo-personal-kb-full)
```

---

## Next Steps

After exploring the demos:

1. **Customize for your needs**: Modify demo code to fit your workflow
2. **Integrate with your tools**: Connect to your existing notes/code
3. **Share with your team**: Use federation for collaborative knowledge
4. **Explore advanced features**: Check out the full documentation

---

## Demo Data

All demos create temporary data in:
- `~/demo-notes/` - Personal knowledge base sample notes
- `~/demo-codebase/` - Sample code for analysis
- `/tmp/meta-log-*` - Temporary files (cleaned up automatically)

Feel free to delete these directories after exploring the demos.

---

## Troubleshooting

**Demo doesn't run:**
```elisp
;; Ensure meta-log is loaded first
(require 'meta-log)
(meta-log-initialize)
```

**Missing dependencies:**
```elisp
;; Check if optional modules are available
(require 'meta-log-federation nil t)
(require 'meta-log-llm nil t)
```

**Want to reset demo state:**
```elisp
;; Reinitialize meta-log
(meta-log-reset)
(meta-log-initialize)
```

---

## Contributing

Have an idea for a new demo? We'd love to see it!

Useful demo topics:
- Academic writing assistant
- Meeting notes organizer
- Technical documentation generator
- Learning path builder
- Project management assistant

See `CONTRIBUTING.md` for guidelines.
