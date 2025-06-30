import os
import requests
from time import sleep
from pathlib import Path


class DocumentationItem:
    def __init__(self, doc_source_url: str, doc_file: str):
        self.doc_source_url: str = doc_source_url
        self.doc_file: str = doc_file


elements_doc_url = "https://raw.githubusercontent.com/togglebyte/anathema-guide/refs/heads/main/src/templates/elements/"
elements_dir = "docs/elements/"

elements = [
    "text",
    "span",
    "border",
    "alignment",
    "vstack",
    "hstack",
    "zstack",
    "row",
    "column",
    "expand",
    "position",
    "spacer",
    "overflow",
    "padding",
    "canvas",
    "container",
]

documentation_item_map = {}
for element in elements:
    documentation_item_map[element] = DocumentationItem(
        elements_doc_url + element + ".md",
        elements_dir + element + ".md",
    )


def main() -> None:
    project_root = get_project_root()
    maybe_make_dir(os.path.join(project_root, "docs"))
    update_element_docs(project_root)


def maybe_make_dir(path: str):
    if not os.path.exists(path):
        Path(path).mkdir(parents=True, exist_ok=True)


def get_project_root() -> str:
    cwd = os.getcwd()
    expected_path = os.path.join("aml_ls", "scripts")

    if not cwd.endswith(expected_path):
        raise RuntimeError("Running update_element_docs script from wrong directory")

    return os.path.dirname(cwd)


def update_element_docs(project_root: str):
    for name, element in documentation_item_map.items():
        print(f"fetching documentation for {name} element")
        doc_string = get_doc_as_string(name, element.doc_source_url)
        doc_path = os.path.join(project_root, element.doc_file)
        update_doc_file(name, doc_path, doc_string)

        # small sleep to not bombard github
        sleep(1)


def get_doc_as_string(element: str, doc_source_url: str):
    try:
        response = requests.get(doc_source_url)
        return response.text
    except Exception:
        print(f"failed to fetch documentation for {element}")
        exit(1)


def update_doc_file(name: str, file_path: str, doc_string: str):
    maybe_make_dir(os.path.dirname(file_path))

    try:
        file = open(file_path, "w")
        written = file.write(doc_string)
        if written != len(doc_string):
            print(f"failed to save full documentation for {name}")
            exit(1)

    except Exception:
        print(f"failed to save documentation for {name}")
        exit(1)


if __name__ == "__main__":
    main()
