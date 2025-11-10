class CorrectUnicorn < Formula
  desc "Passphrase generator inspired by xkcd 936"
  homepage "https://github.com/utgarda/correct-unicorn"
  url "https://github.com/utgarda/correct-unicorn/archive/refs/tags/v0.1.1.tar.gz"
  sha256 "d8a5f672f52a3b2c55c27e8143cb0a267a7e854276b12bcc3cb768910192a4b0"
  license "MIT"
  head "https://github.com/utgarda/correct-unicorn.git", branch: "master"

  depends_on "haskell-stack" => :build
  depends_on "gmp"

  def install
    # Build with Stack
    system "stack", "build", "--install-ghc"

    # Install binary
    bin.install Dir[".stack-work/install/**/bin/correct-unicorn"].first

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
