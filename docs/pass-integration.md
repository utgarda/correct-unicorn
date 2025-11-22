# Pass Integration Guide

correct-unicorn integrates with [pass](https://www.passwordstore.org/), the standard Unix password manager, allowing you to generate and store passphrases in one command.

## Prerequisites

### 1. Install pass

**Arch Linux:**
```bash
sudo pacman -S pass
```

**Ubuntu/Debian:**
```bash
sudo apt-get install pass
```

**macOS:**
```bash
brew install pass
```

### 2. Set up GPG

Pass uses GPG for encryption. If you don't have a GPG key:

```bash
# Generate a new GPG key
gpg --full-generate-key

# Choose RSA and RSA, 4096 bits
# Set expiration (or choose no expiration)
# Enter your name and email
```

Find your GPG key ID:

```bash
gpg --list-secret-keys --keyid-format LONG
```

Look for a line like:
```
sec   rsa4096/ABCD1234EFGH5678 2025-01-01 [SC]
```

The part after `rsa4096/` is your GPG ID.

### 3. Initialize pass

```bash
pass init YOUR_GPG_ID
```

Example:
```bash
pass init ABCD1234EFGH5678
```

### 4. Optional: Git integration

```bash
pass git init
pass git remote add origin git@github.com:username/password-store.git
```

## Usage

### Basic Usage

```bash
# Generate 5-word passphrase and store in pass
correct-unicorn --words 5 --pass github.com/myaccount

# Shorter syntax
correct-unicorn -w 6 -P work/email
```

### With Custom Options

```bash
# Separator and capitalization
correct-unicorn -w 5 -s "-" --capitalize --pass personal/bank

# Minimum character count
correct-unicorn -w 4 -c 25 --pass aws/production

# Quiet mode (no stdout, only pass insertion)
correct-unicorn -w 5 --pass email/gmail --quiet
```

### Force Overwrite

```bash
# Overwrite existing entry without confirmation
correct-unicorn -w 6 --pass github.com/myaccount --pass-force
```

## Workflow Examples

### Creating a New Account

```bash
# Generate passphrase and store
correct-unicorn -w 5 --capitalize --pass websites/example.com

# Retrieve when needed
pass show websites/example.com

# Copy to clipboard (clears after 45 seconds)
pass show -c websites/example.com
```

### Organizing Passwords

Use directory structure for organization:

```
~/.password-store/
├── personal/
│   ├── email/
│   │   ├── gmail.gpg
│   │   └── protonmail.gpg
│   └── banking/
│       └── bank-account.gpg
├── work/
│   ├── email.gpg
│   ├── github.gpg
│   └── aws/
│       ├── dev.gpg
│       └── production.gpg
└── websites/
    ├── reddit.gpg
    └── twitter.gpg
```

Create entries:
```bash
correct-unicorn -w 5 -P personal/email/gmail
correct-unicorn -w 6 -P work/github
correct-unicorn -w 7 -P work/aws/production
```

### Combining with pass Features

correct-unicorn generates the passphrase, pass manages it:

```bash
# Generate and insert
correct-unicorn -w 5 --pass github.com/username

# Edit to add metadata (username, URL, notes)
pass edit github.com/username

# Example multiline entry:
# SuperSecret-Correct-Horse-Battery-Staple
# username: myusername
# url: https://github.com
# 2FA: enabled

# Copy to clipboard
pass show -c github.com/username

# Show full entry
pass show github.com/username
```

## Security & Trust

### Can correct-unicorn read my existing passwords?

**No.** correct-unicorn can only **insert** new passwords into pass, it cannot **read** existing ones.

This is due to how GPG encryption works:
- **Encryption** (inserting): Uses your GPG public key, no password needed
- **Decryption** (reading): Requires your GPG private key passphrase

When correct-unicorn inserts a password, GPG encrypts it without asking for your passphrase. When you read a password with `pass show`, GPG asks for your passphrase to decrypt.

Try it yourself:
```bash
# Inserting - no password prompt
correct-unicorn -w 5 --pass test/demo

# Reading - GPG asks for your passphrase
pass show test/demo
```

### What command does correct-unicorn run?

```bash
pass insert --echo [--force] path/to/entry < passphrase
```

That's it. No reading, no decryption, no access to your other passwords.

**Note:** correct-unicorn writes the passphrase directly to stdin using Haskell's IO system - the passphrase never appears in process arguments or shell history.

### How can I trust this?

**Open source:** The entire codebase is ~1500 lines on [GitHub](https://github.com/utgarda/correct-unicorn). Pass integration is just 150 lines in `src/PassIntegration.hs`.

**No network access:** correct-unicorn doesn't make network calls, doesn't phone home, doesn't send telemetry.

**Same as pass:** correct-unicorn uses the same GPG encryption model as `pass insert` itself.

### Security Contact

Found a security issue? Please report via GitHub Security Advisories or email gene@chainhackers.xyz (PGP preferred).

## Troubleshooting

### "pass is not installed"

Install pass:
- Arch: `sudo pacman -S pass`
- Ubuntu: `sudo apt-get install pass`
- macOS: `brew install pass`

### "pass has not been initialized"

Initialize pass with your GPG key:
```bash
pass init YOUR_GPG_ID
```

### "gpg: signing failed: Inappropriate ioctl for device"

Add to `~/.bashrc` or `~/.zshrc`:
```bash
export GPG_TTY=$(tty)
```

Then reload:
```bash
source ~/.bashrc  # or source ~/.zshrc
```

### "No secret key"

Your GPG key may not be available. Check:
```bash
gpg --list-secret-keys
```

If no keys exist, generate one:
```bash
gpg --full-generate-key
```

Then re-initialize pass:
```bash
pass init YOUR_GPG_ID
```

## Security Considerations

### Why pass?

- **GPG encryption**: Military-grade encryption for all passwords
- **Git integration**: Version control and sync across devices
- **Unix philosophy**: Simple, transparent, scriptable
- **Open source**: Auditable, no vendor lock-in
- **Standard tool**: Used by thousands of developers and sysadmins

### Best Practices

1. **Use strong GPG keys**: 4096-bit RSA minimum
2. **Back up your GPG key**: Store securely offline
3. **Use git sync**: Keep encrypted backups on remote git repo
4. **Regular rotation**: Update critical passwords periodically
5. **Passphrase strength**: Use 5+ words for high-security accounts

### ANSI Codes

correct-unicorn automatically strips ANSI color codes before inserting into pass, ensuring clean storage.

## Advanced Usage

### Scripting

```bash
# Generate passwords for multiple accounts
for site in github.com gitlab.com bitbucket.org; do
    correct-unicorn -w 5 --pass "work/$site" --quiet
done

# Generate with custom dictionary
correct-unicorn -w 6 --dict /usr/share/dict/english --pass custom/entry
```

### Integration with Other Tools

```bash
# Generate and immediately copy to clipboard
correct-unicorn -w 5 --pass temp/throwaway --quiet && pass show -c temp/throwaway

# Generate for ssh key passphrase
correct-unicorn -w 7 -c 35 --capitalize --pass ssh/id_rsa
```

## See Also

- [pass documentation](https://www.passwordstore.org/)
- [pass man page](https://linux.die.net/man/1/pass)
- [Arch Wiki: pass](https://wiki.archlinux.org/title/Pass)
- [xkcd 936: Password Strength](https://xkcd.com/936/)

## References

- [Pass: The Standard Unix Password Manager](https://www.passwordstore.org/)
- [ArchWiki: Pass](https://wiki.archlinux.org/title/Pass)
- [GPG documentation](https://gnupg.org/documentation/)
