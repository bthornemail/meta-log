#!/bin/bash
# meta-log one-command installer
# Usage: curl -fsSL https://raw.githubusercontent.com/bthornemail/meta-log/main/install.sh | bash

set -e

echo "🚀 Installing meta-log - Your AI-Powered Knowledge System"
echo "=========================================================="
echo ""

# Detect platform
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    PLATFORM="linux"
    if [ -n "$TERMUX_VERSION" ]; then
        PLATFORM="termux"
    fi
elif [[ "$OSTYPE" == "darwin"* ]]; then
    PLATFORM="macos"
else
    echo "❌ Unsupported platform: $OSTYPE"
    exit 1
fi

echo "📱 Detected platform: $PLATFORM"

# Installation directory
INSTALL_DIR="$HOME/.meta-log"
mkdir -p "$INSTALL_DIR"

# Clone repository
echo "📥 Downloading meta-log..."
if [ -d "$INSTALL_DIR/repo" ]; then
    echo "   Updating existing installation..."
    cd "$INSTALL_DIR/repo"
    git pull
else
    git clone https://github.com/bthornemail/meta-log.git "$INSTALL_DIR/repo"
fi

# Install Emacs if needed
echo ""
echo "📝 Checking Emacs installation..."
if ! command -v emacs &> /dev/null; then
    echo "   Installing Emacs..."
    case $PLATFORM in
        termux)
            pkg install -y emacs
            ;;
        linux)
            if command -v apt-get &> /dev/null; then
                sudo apt-get update
                sudo apt-get install -y emacs
            elif command -v dnf &> /dev/null; then
                sudo dnf install -y emacs
            elif command -v pacman &> /dev/null; then
                sudo pacman -S --noconfirm emacs
            fi
            ;;
        macos)
            if ! command -v brew &> /dev/null; then
                echo "   Please install Homebrew first: https://brew.sh"
                exit 1
            fi
            brew install emacs
            ;;
    esac
else
    echo "   ✅ Emacs already installed"
fi

# Install optional LLM backend (Ollama recommended)
echo ""
echo "🤖 Would you like to install Ollama for local LLM support? (y/n)"
read -r INSTALL_OLLAMA
if [[ "$INSTALL_OLLAMA" =~ ^[Yy]$ ]]; then
    echo "   Installing Ollama..."
    case $PLATFORM in
        termux)
            echo "   ⚠️  Ollama not yet supported on Termux. You can use API-based LLMs instead."
            ;;
        linux|macos)
            curl -fsSL https://ollama.com/install.sh | sh
            echo "   Downloading gemma2:2b model (best for mobile/low-resource)..."
            ollama pull gemma2:2b
            ;;
    esac
fi

# Create Emacs configuration
echo ""
echo "⚙️  Configuring Emacs..."
EMACS_CONFIG="$HOME/.emacs.d/init.el"
mkdir -p "$HOME/.emacs.d"

if [ ! -f "$EMACS_CONFIG" ]; then
    touch "$EMACS_CONFIG"
fi

# Add meta-log to load path if not already there
if ! grep -q "meta-log" "$EMACS_CONFIG"; then
    cat >> "$EMACS_CONFIG" <<EOF

;; meta-log configuration (added by installer)
(add-to-list 'load-path "$INSTALL_DIR/repo")
(require 'meta-log)

;; Auto-start meta-log dashboard
(add-hook 'emacs-startup-hook 'meta-log-dashboard)
EOF
    echo "   ✅ Added meta-log to Emacs config"
else
    echo "   ✅ meta-log already in Emacs config"
fi

# Create desktop/app launcher
echo ""
echo "🎯 Creating launcher..."
case $PLATFORM in
    linux)
        DESKTOP_FILE="$HOME/.local/share/applications/meta-log.desktop"
        mkdir -p "$HOME/.local/share/applications"
        cat > "$DESKTOP_FILE" <<EOF
[Desktop Entry]
Name=meta-log
Comment=AI-Powered Knowledge System
Exec=emacs --eval "(meta-log-dashboard)"
Icon=emacs
Terminal=false
Type=Application
Categories=Office;Development;
EOF
        echo "   ✅ Created desktop launcher"
        ;;
    macos)
        # Create simple launcher script
        cat > "$HOME/Desktop/meta-log.command" <<EOF
#!/bin/bash
emacs --eval "(meta-log-dashboard)"
EOF
        chmod +x "$HOME/Desktop/meta-log.command"
        echo "   ✅ Created launcher on Desktop"
        ;;
    termux)
        # Create shortcut command
        mkdir -p "$HOME/.shortcuts"
        cat > "$HOME/.shortcuts/meta-log" <<EOF
#!/bin/bash
emacs --eval "(meta-log-dashboard)"
EOF
        chmod +x "$HOME/.shortcuts/meta-log"
        echo "   ✅ Created Termux widget shortcut"
        ;;
esac

# Success message
echo ""
echo "✨ Installation complete!"
echo ""
echo "🎉 Next steps:"
echo "   1. Launch meta-log:"
case $PLATFORM in
    termux)
        echo "      Run: emacs --eval '(meta-log-dashboard)'"
        echo "      Or use Termux:Widget shortcut"
        ;;
    linux)
        echo "      Find 'meta-log' in your applications menu"
        echo "      Or run: emacs --eval '(meta-log-dashboard)'"
        ;;
    macos)
        echo "      Double-click 'meta-log.command' on your Desktop"
        echo "      Or run: emacs --eval '(meta-log-dashboard)'"
        ;;
esac
echo ""
echo "   2. First-time setup wizard will guide you through:"
echo "      • Choosing your LLM backend (local/API)"
echo "      • Importing your notes folder"
echo "      • Setting up your knowledge graph"
echo ""
echo "   3. Documentation: https://github.com/bthornemail/meta-log"
echo ""
echo "📚 Try asking: 'What files mention machine learning?'"
echo ""
