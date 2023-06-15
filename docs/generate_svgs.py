import re
import logging
import subprocess

from mkdocs.structure.pages import Page
from mkdocs.structure.files import Files
from mkdocs.config.defaults import MkDocsConfig


logger = logging.getLogger('mkdocs')
rzk_code_block = re.compile(r'(^```\s*rzk[^\n]*\s+(.*?)\s+^```)', flags=re.MULTILINE | re.DOTALL)
svg_element = re.compile(r'^(<svg.*?</svg>)', flags=re.MULTILINE | re.DOTALL)
rzk_installed = True

try:
    # Capture output to prevent logging usage
    subprocess.run('rzk', capture_output=True)
except FileNotFoundError:
    logger.warn('Rzk executable not found')
    rzk_installed = False


def on_page_markdown(md: str, page: Page, config: MkDocsConfig, files: Files) -> str:
    if not page.file.src_uri.endswith('.rzk.md'): return md
    if not rzk_installed: return md
    # Some snippets can depend on terms defined in previous snippets, so we need to store them all
    previous_snippets = ['#lang rzk-1\n#set-option "render" = "svg"\n\n']
    # Since each snippet will contain previous ones, the previously printed SVGs should not be repeated
    previous_svgs: set[str] = set()
    code_blocks = rzk_code_block.findall(md)
    for (fenced_block, code) in code_blocks:
        previous_snippets.append(code.replace('#lang rzk-1', ''))
        full_code = '\n'.join(previous_snippets).encode()
        process = subprocess.run('rzk typecheck', capture_output=True, input=full_code)
        if process.returncode != 0: continue

        output = process.stderr.decode()
        svgs: list[str] = svg_element.findall(output)
        # One snippet might have more than one diagram, so we shouldn't just use svgs[-1]
        # However, there is probably a more efficient way than iterating over all matches everytime
        for svg in svgs:
            if svg in previous_svgs: continue
            previous_svgs.add(svg)
            md = md.replace(fenced_block, svg + '\n\n' + fenced_block)

    return md
