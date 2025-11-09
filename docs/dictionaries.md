# Dictionary Management

## Auto-Discovery

correct-unicorn searches for system dictionaries using the order defined in `/etc/correct-unicorn/config.toml`.

Default discovery order (shipped with package):

```
/usr/share/dict/english            # Arch (correctpony wordlists)
/usr/share/dict/american-english   # Debian/Ubuntu
/usr/share/dict/words               # Universal fallback/symlink
/usr/dict/words                     # Legacy systems
```

First valid dictionary found is used.

## Discovery Order Configuration

System package includes `/etc/correct-unicorn/config.toml` with:

```toml
# Dictionary auto-discovery paths (checked in order)
dict-paths = [
  "/usr/share/dict/english",
  "/usr/share/dict/american-english",
  "/usr/share/dict/words",
  "/usr/dict/words"
]
```

System administrators can modify this to prioritize different dictionaries.

## Validation

Every use validates the dictionary:

- **Uniqueness check**: All words must be unique (fast: ~3ms for 2k words, <50ms for 100k)
- **Minimum size**: Dictionary must have sufficient words for entropy
- **Readability**: File must be readable, proper format (one word per line)

Validation runs automatically on every password generation (negligible overhead).

## Interactive Mode

```bash
correct-unicorn --interactive
# or
correct-unicorn -i
```

Shows all discovered dictionaries with validation info:

```
Available dictionaries:

  [1] /usr/share/dict/english
      ✓ 2,163 unique words
      ✓ All words validated

  [2] /usr/share/dict/words -> american-english
      ✓ 102,401 unique words
      ✓ All words validated

Select dictionary [1-2] or specify path:
```

Interactive mode always available for transparency.

## Manual Selection

```bash
# Use specific dictionary
correct-unicorn --dict /usr/share/dict/british-english

# Check if dictionary is valid
correct-unicorn --dict /path/to/wordlist.txt --validate
```

## Password Complexity Requirements

Some systems require special characters and numbers. Instead of randomly mangling words (ruining memorability), use controlled minimal substitutions:

```bash
# Apply minimum required character substitutions
correct-unicorn --require-special-chars
# Example: "correct horse battery staple" → "c0rrect horse b@ttery staple"
# Only substitutes ONE 'o'→'0' and ONE 'a'→'@' to satisfy requirements
```

Substitution priority (defined in `/etc/correct-unicorn/config.toml`):
1. First 'o' → '0' (for number requirement)
2. First 'a' → '@' (for special char requirement)
3. Stops when requirements met

Predictable, memorable, satisfies arbitrary complexity rules.

## Requirements

Dictionary must:
- Be plain text (ASCII or UTF-8)
- One word per line
- All words unique
- Minimum 1000 words (recommended 10,000+)

## Installation

See [installation.md](installation.md) for platform-specific dictionary package installation.
