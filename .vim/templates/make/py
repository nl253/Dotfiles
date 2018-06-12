
SHELL=/bin/bash

venv-init:
	python -m venv ./.pyvenv

requirements: venv
	pip install --force -U --user --pre -r requirements.txt 
