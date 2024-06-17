# Common Lisp Alive and Jupyter extensions in VSCode

The installation follows the steps:

* Install `roswell`
* Install `common-lisp-jupyter`
* Install `alive`
* Configure `alive`

## Roswell

There is no installer for `roswell` in the debian repository.
The easiest way is to compile from scratch using `git` sources.

The script below uses `sbcl-bin/2.4.2` in order to use the jupyter kernel.

```bash
# Create git installation directory.
cd
mkdir -p git
cd git
# Install prerequisites
sudo apt-get -y install git build-essential automake libcurl4-openssl-dev
git clone -b release https://github.com/roswell/roswell.git
cd roswell
sh bootstrap
./configure
make
sudo make install
# Install environment under $USER/.roswell
ros setup
# Select a working SBCL.
ros install sbcl-bin/2.4.2
ros use sbcl-bin/2.4.2
# Add roswell to the PATH
echo 'export PATH=$PATH:~/.roswell/bin' >> ~/.bashrc
```

## Common-Lisp-Jupyter

This works only with `sbcl-bin/2.4.2` under roswell.

```bash
# Install prerequisites.
sudo apt install jupyter libczmq-dev

# Install the jupyter kernel.
# This installs a kernel in:
# ~/.local/share/jupyter/kernels/common-lisp/kernel.json
ros install common-lisp-jupyter
```

Copy the following file
(replacing `/home/user/` with the correct home directory):

```json
{
  "argv": [
    "/home/user/.roswell/bin/cl-jupyter",
    "{connection_file}"
  ],
  "display_name": "Common Lisp",
  "language": "common-lisp",
  "interrupt_mode": "message",
  "metadata": {
    "debugger": true
  },
  "env": {
    "PATH": "${PATH}:${HOME}/.roswell/bin"
  }
}
```

to:
`~/.local/share/jupyter/kernels/common-lisp/kernel.json`

This allows to start the common-lisp-server using `jupyter lab` and use *Common Lisp* in a notebook. If editing a CL notebook in vscode, select the Common Lisp kernel. The hardcoded path to `cl-jupyter` is necessary because the vscode jupyter version would forget the environment variables assignments.

The `jupyterlab-debugger-restarts` seems unmaintained and no longer installable.

## Alive

Install alive-lsp as a local quicklisp project.

```bash
cd ~/.roswell/lisp/quicklisp/local-projects
git clone https://github.com/nobody-famous/alive-lsp.git
```

Test the alive server using:

```bash
ros run --eval '(require :asdf)' --eval '(asdf:load-system :alive-lsp)' --eval '(alive/server:start)'
```

Add the proper *alive* start command into the *settings.json*:

```json
{
  "alive.lsp.startCommand": [
    "ros",
    "run",
    "--eval",
    "(require :asdf)",
    "--eval",
    "(asdf:load-system :alive-lsp)",
    "--eval",
    "(alive/server:start)"
  ],
}
```
