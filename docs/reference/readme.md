## Viewing the Documentation

The documentation is written in [AsciiDoc](https://asciidoc.org/) (`.adoc`).
You can view it in several ways:

* View in IDE
* Convert to HTML
* Convert to PDF

---

## View in IDE

Install an AsciiDoc plugin for your IDE:

* **VS Code**: *AsciiDoc by asciidoctor*
* **IntelliJ IDEA**: *AsciiDoc Plugin*

After installing the plugin, open any `.adoc` file in `docs/reference/src/components/cairo/modules`.

---

## View as HTML

### Install `asciidoctor`

**macOS:**

```bash
brew install asciidoctor
```

**Ubuntu:**

```bash
sudo apt-get install asciidoctor
```

### Convert to HTML

> **Tip:** `cd docs/reference`

**Convert a single file:**

```bash
asciidoctor src/components/cairo/modules/ROOT/pages/index.adoc
```

**Convert all `.adoc` files:**

```bash
asciidoctor src/components/cairo/modules/*/pages/*.adoc
```

HTML files will be created in the same directory as the source files.

---

## View as PDF

### Install `asciidoctor-pdf`

> **Note:** `asciidoctor-pdf` is **not installed automatically** with `asciidoctor`.

**macOS:**

```bash
brew install asciidoctor-pdf
```

**Ubuntu:**
(`asciidoctor-pdf` is not always available in apt, so RubyGems is used)

```bash
sudo apt-get install ruby ruby-dev
sudo gem install asciidoctor-pdf
```

### Convert to PDF

> **Tip:** `cd docs/reference`

**Convert a single file:**

```bash
asciidoctor-pdf src/components/cairo/modules/ROOT/pages/index.adoc
```

**Convert all `.adoc` files:**

```bash
asciidoctor-pdf src/components/cairo/modules/*/pages/*.adoc
```

PDF files will be created next to the source `.adoc` files.
