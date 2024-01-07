# Rzk blog

## Setup

When setting up MkDocs for the first time, it is recommended to create a Python venv and activate it:
```bash
python -m venv venv
venv\Scripts\Activate.ps1
```

The activation command will depend on the OS. The command shown above is for PowerShell on Windows.

Then, install the requirements:
```bash
pip install -r docs/requirements.txt
```

## Running

Activate the venv (Windows - PowerShell):
```bash
venv\Scripts\Activate.ps1
```

Run the dev server:
```bash
mkdocs serve --config-file docs/config/en/mkdocs.yml
```
