# Configuration

Two configuration files:

## System Configuration

```
/etc/correct-unicorn/config.toml
```

Shipped with package. Defines dictionary discovery order:

```toml
dict-paths = [
  "/usr/share/dict/english",
  "/usr/share/dict/american-english",
  "/usr/share/dict/words",
  "/usr/dict/words"
]
```

System administrators can modify for local preferences.

## User Configuration

```
~/.config/correct-unicorn/config.toml
```

**Optional**. UI preferences only:

## What Goes in Config

**UI preferences only** (does not affect password security):
- Colors
- Separator style
- Bold formatting

**What does NOT go in config:**
- Dictionary path (use `--dict` flag or auto-discovery)
- Word count (use `--words` flag)
- Any security-critical settings

## Format

```toml
colors = ["green", "yellow", "blue", "magenta"]
separator = ""
bold = true
```

See [example-config.toml](example-config.toml) for complete example.

## Dictionaries

Dictionary selection and validation happens at runtime, not via config file.

See [dictionaries.md](dictionaries.md) for:
- Auto-discovery
- Validation (uniqueness, minimum size)
- Interactive mode to view and select dictionaries
