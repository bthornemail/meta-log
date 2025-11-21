# 🚀 meta-log Quick Start Guide

Get up and running with meta-log in **under 5 minutes**.

## What is meta-log?

meta-log is an AI-powered personal knowledge system that:
- 📚 Searches all your notes instantly
- 🤖 Answers questions using AI + your knowledge
- 🔗 Finds connections between ideas automatically
- 💬 Works like ChatGPT, but with **your** data

## Installation

### One-Command Install (Recommended)

```bash
curl -fsSL https://raw.githubusercontent.com/bthornemail/meta-log/main/install.sh | bash
```

This installs:
- ✅ Emacs (if needed)
- ✅ meta-log
- ✅ Ollama (optional, for local AI)
- ✅ Desktop launcher

**Time:** ~3 minutes

### Manual Install

```bash
# Clone repository
git clone https://github.com/bthornemail/meta-log.git
cd meta-log

# Add to Emacs config
echo "(add-to-list 'load-path \"$(pwd)\")" >> ~/.emacs.d/init.el
echo "(require 'meta-log)" >> ~/.emacs.d/init.el
echo "(meta-log-dashboard)" >> ~/.emacs.d/init.el
```

**Time:** ~1 minute

## First Launch

After installation, launch meta-log:

### Linux
Find **"meta-log"** in your applications menu, or run:
```bash
emacs --eval "(meta-log-dashboard)"
```

### macOS
Double-click **"meta-log.command"** on your Desktop, or run:
```bash
emacs --eval "(meta-log-dashboard)"
```

### Termux (Android)
```bash
emacs --eval "(meta-log-dashboard)"
```
Or use the Termux:Widget shortcut.

## Setup Wizard (First Time Only)

The setup wizard guides you through 3 simple steps:

### Step 1: Choose AI Backend

Pick how you want the AI to work:

**Option A: Local AI (Ollama)** ⭐ Recommended
- ✅ Free, private, runs on your device
- ✅ No internet required after setup
- ⚠️  Requires 2GB disk space, 4GB RAM

**Option B: Cloud AI (OpenAI/Anthropic)**
- ✅ More powerful, faster
- ⚠️  Requires API key & internet

**Option C: No AI**
- ✅ Pure logic programming (Prolog/Datalog)
- ⚠️  No natural language queries

**Recommendation:** Choose Ollama if you have 4GB+ RAM. Otherwise, use Cloud AI with a free API key.

### Step 2: Import Your Notes

Point meta-log to your notes folder (e.g., `~/Documents/Notes`).

Supported formats:
- `.md` (Markdown)
- `.org` (Org Mode)
- `.txt` (Plain Text)

The wizard will:
1. Scan all files
2. Extract text and metadata
3. Build searchable index
4. Create knowledge graph

**Time:** Depends on # of files (typically 10-60 seconds for 100-1000 files)

### Step 3: Done!

You're ready to use meta-log! 🎉

## Using meta-log

### Method 1: Chat Interface (Easiest)

Press `c` in the dashboard, or run:
```elisp
M-x meta-log-chat
```

Type questions like:
- "What files mention machine learning?"
- "Show me notes about Prolog"
- "What are the main topics in my notes?"
- "Explain church encoding from my notes"

### Method 2: Dashboard

Press `g` to refresh the dashboard and see:
- 📊 Knowledge graph stats
- 🔍 Quick search
- 📥 Import more files

### Method 3: Direct Commands

```elisp
M-x meta-log-ask RET What files mention Holochain?
M-x meta-log-search RET machine learning
M-x meta-log-ingest-folder RET ~/Documents/NewNotes
```

## Keyboard Shortcuts

### In Dashboard
- `c` - Open chat
- `i` - Import notes folder
- `s` - Search knowledge base
- `v` - View knowledge graph
- `g` - Refresh dashboard
- `h` - Help
- `q` - Quit

### In Chat
- `RET` - Send message
- `C-c C-c` - Send message (alternative)
- `C-c C-l` - Clear chat history
- `C-c C-s` - Save chat to file
- `C-c C-q` - Quit chat

## Configuration

All settings are in: `~/.meta-log/config.el`

### Change AI Backend

```elisp
;; Use Ollama
(setq meta-log-llm-backend 'ollama)

;; Use OpenAI
(setq meta-log-llm-backend 'api)
(setq meta-log-api-provider 'openai)
(setq meta-log-api-key "sk-...")

;; Use Anthropic
(setq meta-log-llm-backend 'api)
(setq meta-log-api-provider 'anthropic)
(setq meta-log-api-key "sk-ant-...")
```

### Change Notes Folder

```elisp
(setq meta-log-notes-folder "~/Dropbox/Notes")
```

### Enable/Disable Auto-Import

```elisp
(setq meta-log-auto-ingest t)  ; Auto-import new files
(setq meta-log-auto-ingest nil) ; Manual import only
```

Re-run setup wizard anytime:
```elisp
M-x meta-log-setup
```

## Troubleshooting

### "No LLM configured"
- Run `M-x meta-log-setup` to configure AI backend
- Or set manually in `~/.meta-log/config.el`

### "No notes found"
- Import notes: `M-x meta-log-ingest-folder`
- Check your notes folder path in settings

### Ollama model download fails
```bash
# Check Ollama is running
ollama serve

# Download model manually
ollama pull gemma2:2b
```

### Chat doesn't respond
- Check AI backend is configured
- For Ollama: ensure `ollama serve` is running
- For Cloud API: verify API key is valid

## Advanced Features

Once comfortable with basics, explore:

### Prolog Queries
```elisp
M-x meta-log-prolog-query RET node(?Id, ?Type)
```

### Datalog Queries
```elisp
M-x meta-log-datalog-query RET node(?Id, text)
```

### M-Expressions
```elisp
M-x meta-log-m-expr-eval RET query[find[all[nodes]]]
```

### Knowledge Graph Visualization
```elisp
M-x meta-log-kg-visualize
```

## What's Next?

- 📖 **Full Documentation:** [docs/USER_GUIDE.md](docs/USER_GUIDE.md)
- 🏗️  **Architecture:** [docs/architecture/](docs/architecture/)
- 💡 **Examples:** [examples/](examples/)
- 🤝 **Federation:** [docs/FEDERATION_GUIDE.md](docs/FEDERATION_GUIDE.md)
- 🔐 **Cryptography:** [docs/CRYPTO_GUIDE.md](docs/CRYPTO_GUIDE.md)

## Getting Help

- 🐛 **Report Issues:** https://github.com/bthornemail/meta-log/issues
- 💬 **Discussions:** https://github.com/bthornemail/meta-log/discussions
- 📧 **Email:** bthornemail@gmail.com

## Tips for Best Results

1. **Organize your notes** - Use descriptive filenames and clear titles
2. **Use tags** - Add `#tags` to make searching easier
3. **Link concepts** - Reference related ideas in your notes
4. **Regular imports** - Enable auto-import or manually import new notes
5. **Experiment** - Try different question phrasings to see what works best

---

**You're all set!** 🎉

Start chatting: `M-x meta-log-chat`

Happy knowledge building! 🚀
