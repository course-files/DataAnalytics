# Installation of Multiple Python Interpreter Versions

## <img src="https://cdn.jsdelivr.net/gh/devicons/devicon@latest/icons/windows11/windows11-original.svg" width="40" /> Installing Python in Windows

Download the versions you desire from here: [https://www.python.org/downloads/windows/](https://www.python.org/downloads/windows/)

Select the following options (if available):

- Add python.exe to PATH
![Screenshot 1](https://raw.githubusercontent.com/course-files/classlab/refs/heads/main/assets/images/PythonForWindows-1.png)

- Customize installation
  - Maintain the defaults for 'Documentation', 'pip', 'tcl/tk and IDLE', and 'Python test suite', i.e., **Enabled**
  ![Screenshot 2](https://raw.githubusercontent.com/course-files/classlab/refs/heads/main/assets/images/PythonForWindows-2.png)
  - Maintain the defaults for 'py launcher' and 'for all users', i.e., **Disabled**

Under advanced options:

- **Enable** 'Install Python for all users', 'Create shortcuts for installed applications', 'Add Python to environment variables', 'Precompile standard library'
- Choose a clear path, preferably one without spaces, example, `C:\Python312\` instead of `C:\Program Files\Python312`
![Screenshot 3](https://raw.githubusercontent.com/course-files/classlab/refs/heads/main/assets/images/PythonForWindows-3.png)

## <img src="https://cdn.jsdelivr.net/gh/devicons/devicon@latest/icons/linux/linux-original.svg" width="40" /> Install Python in Linux

### Install Dependencies

```shell
sudo apt update
sudo apt install -y \
  build-essential \
  libssl-dev \
  zlib1g-dev \
  libbz2-dev \
  libreadline-dev \
  libsqlite3-dev \
  libncursesw5-dev \
  xz-utils \
  tk-dev \
  libxml2-dev \
  libxmlsec1-dev \
  libffi-dev \
  liblzma-dev
```

```shell
sudo apt install git
```

### Install `pyenv`

```shell
curl https://pyenv.run | bash
```

### Edit ~/.bashrc

```shell
vim ~/.bashrc
```

Add:

```shell
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv init -)"
```

Restart the terminal and confirm that `pyenv` is installed:

```shell
pyenv --version
```

### Install the Python Version you Desire

Example:

```shell
pyenv install 3.12.10
```

```shell
pyenv install 3.14.3
```

### Confirm the Python version installed

```shell
pyenv versions
```

### Set the global and local defaults

Global default:

```shell
pyenv global 3.12.10
```

Example of setting the local default (per project):

```shell
cd my_project
pyenv local 3.12.0
```

### Confirm the global Python version installed

```shell
python --version
```

## <img src="https://cdn.jsdelivr.net/gh/devicons/devicon@latest/icons/apple/apple-original.svg" width="40"/> Installing Python in MacOS

Download the versions you desire from here: [https://www.python.org/downloads/macos/](https://www.python.org/downloads/macos/)
