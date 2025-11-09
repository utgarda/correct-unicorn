# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

correct-unicorn is a Haskell-based passphrase generator inspired by https://github.com/maandree/correctpony and xkcd 936. It generates random passphrases by selecting words from a system dictionary and displaying them with ANSI color formatting.

## Design Philosophy

- **Security-first**: Configuration separates security-critical settings (dictionary, word count) from UI preferences (colors, separator)
- **Reliability**: Auto-validates dictionaries, prevents human error in sysadmin tasks
- **Retro hacker aesthetics**: Terminal UI optimized for traditional Linux terminal look (coolretroterm as vibe reference)
- **Traditional Linux terminal UI**: Clipboard integration, pass password manager integration, pipe-friendly output
- **Transparency**: Interactive mode always available to show dictionary options and validation status
- **Minimal complexity requirements**: Controlled character substitution (e.g., one 'o'→'0', one 'a'→'@') instead of random mangling

## Build System

Stack with resolver **LTS 23.25** (GHC 9.8.4)

### Common Commands

- Build: `stack build`
- Run: `stack exec -- correct-unicorn --words 4`
- Test: `stack test`
- Clean: `stack clean`

## Code Architecture

### Library (src/)
- **CorrectUnicorn.hs**: Core logic, configuration types, password generation
- **PrettyAnsi.hs**: ANSI escape codes, color cycling, bold formatting

### Executable (app/)
- **Main.hs**: Thin wrapper, argument parsing entry point

### Tests (test/)
- **Spec.hs**: Test harness (tasty framework)
- **PrettyAnsiTests.hs**: Unit tests for color cycling
- **CorrectUnicornTests.hs**: Tests for core logic

## Configuration

### System Config: `/etc/correct-unicorn/config.toml`
Shipped with package. Defines:
- Dictionary auto-discovery paths order
- Minimum dictionary requirements
- Character substitution rules for complexity requirements

### User Config: `~/.config/correct-unicorn/config.toml` (optional)
UI preferences only:
- Colors
- Separator style
- Bold formatting

**Security-critical settings** (dictionary path, word count) use CLI flags only.

## Key Design Decisions

- **TOML config format**: Available in LTS 23+ via tomland, simpler than YAML
- **Dictionary auto-discovery**: Checks `/usr/share/dict/english`, `/usr/share/dict/american-english`, `/usr/share/dict/words`
- **Fast validation**: Uniqueness check runs every use (3ms for 2k words, <50ms for 100k)
- **Minimal substitutions**: Only replaces minimum characters needed for complexity rules (memorable)
- **No bundled wordlists**: Rely on system packages (words/wamerican), validate automatically

## Platform Support

- **Arch Linux**: Package `words`, provides `/usr/share/dict/english`
- **Ubuntu 22.04/24.04 LTS**: Package `wamerican`, provides `/usr/share/dict/american-english`
- **macOS**: Dictionary pre-installed at `/usr/share/dict/words`

## Git & Commit Guidelines

- **ALWAYS run tests before committing**: `stack test`
- **NEVER use `git add -A` or `git add .`** - specify each file explicitly
  - Example: `git add file1.hs file2.hs file3.hs`
- **Write concise commit messages** - avoid filler words like "comprehensive", "proper", "overall"
- **Do NOT add Claude Code attribution** in commits
- **Keep commit messages in one line** unless detailed explanation needed
- Example: `upgrade to LTS 23.25, add TOML config`
- Not: `Add comprehensive TOML configuration system with proper validation`

## Documentation

See `docs/` directory:
- **configuration.md**: Config file format and locations
- **dictionaries.md**: Auto-discovery, validation, interactive mode
- **installation.md**: Platform-specific installation instructions
- **features.md**: Roadmap and feature planning
- **packaging/**: Arch AUR, Debian package specs
