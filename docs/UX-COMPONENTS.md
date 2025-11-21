# meta-log UX Components

This document explains the new user experience components that make meta-log accessible to regular users.

## Component Overview

```
meta-log/
├── install.sh              # One-command installer
├── test-install.sh         # Installation verification
├── QUICKSTART.md           # 5-minute getting started guide
└── UX Modules:
    ├── meta-log-setup.el      # First-time setup wizard
    ├── meta-log-dashboard.el  # Main dashboard UI
    ├── meta-log-chat.el       # Chat interface
    └── meta-log-ingest.el     # Folder ingestion
```

---

## 1. `install.sh` - One-Command Installer

**Purpose:** Get meta-log installed and configured in < 5 minutes with one command.

**What it does:**
1. Detects platform (Linux/macOS/Termux)
2. Installs Emacs if not present
3. Clones meta-log repository to `~/.meta-log/repo`
4. Optionally installs Ollama + downloads model
5. Configures Emacs (`~/.emacs.d/init.el`)
6. Creates platform-appropriate launcher

**Platform Support:**
- **Linux:** Desktop file → Application menu
- **macOS:** `.command` file → Desktop
- **Termux:** Shortcut → Termux:Widget

**Usage:**
```bash
curl -fsSL https://raw.githubusercontent.com/bthornemail/meta-log/main/install.sh | bash
```

**Entry Point:** Modifies `~/.emacs.d/init.el` to load meta-log and auto-run dashboard

---

## 2. `meta-log-setup.el` - Interactive Setup Wizard

**Purpose:** Guide first-time users through configuration without reading docs.

**Flow:**

```
Welcome Screen
    ↓
Step 1: Choose AI Backend
    - Ollama (local, private)
    - OpenAI/Anthropic (cloud, powerful)
    - None (logic only)
    ↓
Step 2: Import Notes Folder
    - Browse to folder
    - Enable/disable auto-import
    ↓
Step 3: Build Knowledge Graph
    - Process files
    - Save config
    - Launch dashboard
```

**Widgets Used:**
- `radio-button-choice` - Select AI backend
- `editable-field` - Enter paths, API keys
- `checkbox` - Toggle options
- `push-button` - Navigation

**Output:** Saves configuration to `~/.meta-log/config.el`

**Entry Point:**
```elisp
M-x meta-log-setup
```

Auto-runs on first launch if no config exists.

---

## 3. `meta-log-dashboard.el` - Main Dashboard UI

**Purpose:** Replace raw Emacs buffers with a clean, visual interface.

**Layout:**

```
╔═══════════════════════════════════════════╗
║  meta-log — Your AI Knowledge System      ║
╚═══════════════════════════════════════════╝

┌─ System Status ──────────────────────────┐
│  📁 Indexed Files:      42                │
│  🔗 Knowledge Nodes:    127               │
│  ⚡ Connections:        215               │
│  🤖 AI Backend:         ollama            │
└──────────────────────────────────────────┘

┌─ Quick Actions ──────────────────────────┐
│  💬 Ask a Question            [button]   │
│  📥 Import Notes Folder       [button]   │
│  🔍 Search Knowledge Base     [button]   │
│  📊 View Knowledge Graph      [button]   │
│  ⚙️  Settings & Configuration [button]   │
└──────────────────────────────────────────┘

┌─ Recent Activity ────────────────────────┐
│  • What files mention Holochain?         │
│  • Show me notes about Prolog            │
└──────────────────────────────────────────┘
```

**Keyboard Shortcuts:**
- `c` - Chat
- `i` - Import folder
- `s` - Search
- `v` - View knowledge graph
- `g` - Refresh
- `h` - Help
- `q` - Quit

**Entry Point:**
```elisp
M-x meta-log-dashboard
```

Set as default on launch in installer.

---

## 4. `meta-log-chat.el` - Chat Interface

**Purpose:** Natural language queries as the primary interaction mode.

**Features:**
- ChatGPT-style conversation window
- User messages in cyan, assistant in green
- Real-time typing indicator
- Protected history (can't edit past messages)
- Save conversation to file

**Flow:**

```
User types: "What files mention machine learning?"
    ↓
meta-log-chat-get-response()
    ↓
1. Search knowledge graph for context
2. Build prompt with context
3. Query LLM (or fallback to keyword search)
    ↓
Display response
```

**Fallback Behavior:**
If no LLM configured:
- Falls back to grep-based keyword search
- Still shows relevant files
- Suggests configuring AI for better results

**Keyboard Shortcuts:**
- `RET` or `C-c C-c` - Send message
- `C-c C-l` - Clear chat
- `C-c C-s` - Save chat to file
- `C-c C-q` - Quit

**Entry Point:**
```elisp
M-x meta-log-chat
```

---

## 5. `meta-log-ingest.el` - Folder Ingestion

**Purpose:** Turn "add my notes folder" into a one-click operation.

**What it does:**

```
meta-log-ingest-folder ~/Documents/Notes
    ↓
1. Find all .md/.org/.txt files recursively
    ↓
2. For each file:
   - Read content
   - Extract metadata (title, tags, modified)
   - Generate node ID
   - Add to knowledge graph
   - Extract entities via LLM (if available)
   - Create connections
    ↓
3. Build knowledge graph connections
    ↓
4. Done! Show stats
```

**Progress Display:**
```
╔══════════════════════════════════════════╗
║  📥  Importing Notes into meta-log       ║
╚══════════════════════════════════════════╝

🔍 Scanning for files...
   Found 127 files

📚 Processing files...
   [1/127] notes.md ✓
   [2/127] ideas.org ✓
   ...

🔗 Building knowledge graph... ✓

✨ Import Complete! ✨
  • Processed: 127 files
  • Knowledge nodes: 843
  • Connections: 1204
```

**Auto-Import:**
Optional file watcher that auto-imports when new files are added:
```elisp
(setq meta-log-auto-ingest t)
```

**Entry Point:**
```elisp
M-x meta-log-ingest-folder
```

---

## 6. `test-install.sh` - Installation Verification

**Purpose:** Verify installation works before the user tries it.

**Tests:**
1. ✓ Emacs installed?
2. ✓ meta-log files present?
3. ✓ UX modules exist?
4. ⚠ Ollama installed? (optional)
5. ⚠ Guile installed? (optional)
6. ✓ Emacs can load modules?
7. ✓ Configuration exists?
8. ⚠ Launcher created? (optional)

**Usage:**
```bash
./test-install.sh
```

**Output:**
- Green ✓ for passed tests
- Red ✗ for failed tests
- Yellow ⚠ for warnings (optional features)

---

## Component Dependencies

```
meta-log.el (core)
    ├── meta-log-setup.el
    │   ├── widget.el
    │   └── wid-edit.el
    │
    ├── meta-log-dashboard.el
    │   ├── tabulated-list.el
    │   ├── meta-log-ingest.el (optional)
    │   ├── meta-log-chat.el (optional)
    │   └── meta-log-knowledge-graph.el (optional)
    │
    ├── meta-log-chat.el
    │   ├── meta-log-llm.el (optional)
    │   └── meta-log-knowledge-graph.el (optional)
    │
    └── meta-log-ingest.el
        ├── meta-log-knowledge-graph.el (optional)
        ├── meta-log-llm.el (optional)
        └── filenotify.el (for auto-import)
```

**Note:** All UX components gracefully degrade if optional deps are missing.

---

## Design Principles

### 1. **Defaults over Configuration**
- Auto-detect Ollama
- Auto-find common notes folders
- Sane defaults that work out-of-box

### 2. **Progressive Disclosure**
- Simple → Advanced
- Chat first, Prolog later
- Hide complexity until needed

### 3. **Visual Feedback**
- Progress indicators during slow operations
- Clear success/error messages
- Stats always visible (dashboard)

### 4. **Graceful Degradation**
- No LLM? Fall back to keyword search
- No KG? Fall back to file search
- No notes? Guide user to import

### 5. **Keyboard + Mouse**
- Click buttons OR use shortcuts
- Navigate with arrows OR TAB
- Both work equally well

---

## User Journey

### First-Time User (Never Used Emacs)

```
1. Run install script
   Time: 3-5 min

2. Click launcher → Dashboard appears

3. Setup wizard auto-runs
   - Choose "Ollama" (recommended)
   - Browse to ~/Documents
   - Wait 30 sec while importing

4. Click "Start Chatting"
   - Type: "What files mention X?"
   - Get instant answer

5. 🎉 SUCCESS - User is productive
   Total time: < 10 minutes
```

### Power User (Knows Emacs)

```
1. M-x package-install-file RET meta-log.el

2. M-x meta-log-setup
   - Choose "Cloud AI" + API key
   - Point to Org-roam folder
   - Done

3. Continue using Emacs normally
   - M-x meta-log-ask for quick queries
   - C-c C-a in any buffer
   - Prolog/Datalog still available
```

---

## Customization Hooks

Users can customize UX components:

### Dashboard
```elisp
;; Change refresh interval
(setq meta-log-dashboard-refresh-interval 60)  ; seconds

;; Add custom quick action
(defun my-meta-log-action ()
  (interactive)
  (message "My custom action!"))

;; Bind to dashboard
(define-key meta-log-dashboard-mode-map (kbd "m") 'my-meta-log-action)
```

### Chat
```elisp
;; Custom system prompt
(setq meta-log-chat-system-prompt
      "You are a helpful assistant specializing in...")

;; Custom response processing
(add-hook 'meta-log-chat-response-hook 'my-process-response)
```

### Ingestion
```elisp
;; Add custom file types
(add-to-list 'meta-log-ingest-supported-extensions "pdf")

;; Custom metadata extractor
(defun my-extract-metadata (file content)
  ...)

(setq meta-log-ingest-metadata-function 'my-extract-metadata)
```

---

## Testing Checklist

Before releasing UX changes:

- [ ] Fresh Linux install → installer → dashboard → chat
- [ ] Fresh macOS install → installer → dashboard → chat
- [ ] Termux install → installer → dashboard → chat
- [ ] No Emacs installed → installer adds it
- [ ] No Ollama installed → works with fallback
- [ ] Large notes folder (1000+ files) → ingestion completes
- [ ] No internet → local mode works
- [ ] API key invalid → helpful error message
- [ ] Dashboard keyboard shortcuts work
- [ ] Chat history protected from edits
- [ ] Setup wizard saves config correctly

---

## Future Enhancements

Ideas for improving UX further:

1. **Web Interface**
   - Serve dashboard on localhost:4280
   - Access from browser (no Emacs needed)

2. **Mobile App**
   - Native Android/iOS wrapper
   - Voice input for chat

3. **Visual Knowledge Graph**
   - Interactive D3.js/Graphviz view
   - Click to explore

4. **Onboarding Tour**
   - Highlight features on first launch
   - "Try clicking here..."

5. **Templates**
   - "Import Obsidian vault"
   - "Import Org-roam"
   - "Import Logseq graph"

6. **AI Suggestions**
   - "I noticed you have notes about X and Y. Want to explore connections?"

---

## Contributing

To add new UX components:

1. **Follow the pattern:**
   - `meta-log-COMPONENT.el`
   - Interactive entry point: `M-x meta-log-COMPONENT`
   - Graceful degradation if deps missing
   - Add to dashboard as button
   - Document in this file

2. **Test on all platforms:**
   - Linux (Ubuntu/Fedora)
   - macOS (Intel + M1)
   - Termux (Android)

3. **Update:**
   - `QUICKSTART.md` if user-facing
   - `test-install.sh` if testable
   - This document

4. **Keep it simple:**
   - Fewer options > more options
   - Auto-detect > ask user
   - Show progress > silent processing

---

**Remember:** The goal is to make meta-log usable by regular smart developers, not just geniuses. Every component should answer: "Can my friend install and use this without my help?"
