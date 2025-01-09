The docs are written in [AsciiDoc](https://asciidoc.org/)(`.adoc`). You can view the docs locally in many ways.

- View on IDE
- View in HTML
- View in PDF

## View on IDE

- Install `AsciiDoc`plugins in `VSCode` or `Intellij IDEA`

## View in HTML
### Install `asciidoctor`

- Mac OS
```bash
brew install asciidoctor
```

- Ubuntu
```bash
apt-get install asciidoctor
```

### Convert to HTML
> Tip:`pwd` is `cairo/docs/reference`

- Convert a single file
```bash
asciidoctor src/Summary.adoc
```

- Convert all files
```bash
asciidoctor src/*.adoc
```

Now `HTML` versions of the documentation will be generated.

## View in PDF
### Install `asciidoctor`
> Note: when you install `asciidoctor`, `asciidoctor-pdf` also be installed.

- Mac OS
```bash
brew install asciidoctor
```

- Ubuntu
```bash
apt-get install asciidoctor
```

### Convert to PDF
> Tip: `pwd` is `cairo/docs/reference`

- Convert a single file
```bash
asciidoctor-pdf src/Summary.adoc
```

- Convert all files
```bash
asciidoctor-pdf src/*.adoc
```

Now `PDF` versions of the documentation will be generated.
