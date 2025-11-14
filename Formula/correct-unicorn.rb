class CorrectUnicorn < Formula
  desc "Passphrase generator inspired by xkcd 936"
  homepage "https://github.com/utgarda/correct-unicorn"
  url "https://github.com/utgarda/correct-unicorn/archive/refs/tags/v0.1.3.tar.gz"
  sha256 "398a44d922a19e24b4260bee907f08a1e9faecf4f2ef7475d8edf277fd3daf28"
  license "MIT"
  head "https://github.com/utgarda/correct-unicorn.git", branch: "master"

  depends_on "haskell-stack" => :build
  depends_on "gmp"

  def install
    # Build with Stack
    system "stack", "build", "--install-ghc"

    # Install binary
    bin.install Dir[".stack-work/install/**/bin/correct-unicorn"].first

    # Install system config
    (etc/"correct-unicorn").install "etc/correct-unicorn/config.toml"

    # Install example config
    (share/"correct-unicorn").install "docs/example-config.toml"

    # Install documentation
    doc.install Dir["docs/*.md"]
    doc.install "README.md", "LICENSE", "ChangeLog.md"
  end

  def caveats
    <<~EOS
      Example configuration has been installed to:
        #{share}/correct-unicorn/example-config.toml

      To use it, copy to your home directory:
        mkdir -p ~/.config/correct-unicorn
        cp #{share}/correct-unicorn/example-config.toml ~/.config/correct-unicorn/config.toml

      System-wide configuration can be placed at:
        /etc/correct-unicorn/config.toml
    EOS
  end

  test do
    # Test that the binary runs and generates output
    output = shell_output("#{bin}/correct-unicorn -w 4 -p")
    assert_match(/\w+/, output, "Should generate words")

    # Test help command
    assert_match "passphrase generator", shell_output("#{bin}/correct-unicorn --help")
  end
end
