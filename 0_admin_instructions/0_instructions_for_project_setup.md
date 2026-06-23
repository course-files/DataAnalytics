# The Virtual Environment (`.venv`) and the List of Dependencies Folder (`./requirements`)

## Project Setup Instructions

### Install all the dependencies listed in the `requirements` folder depending on the environment

1. Confirm that you have Python installed. You can check this by running:

    ```shell
    python --version
    ```

    or

    ```shell
    python3 --version
    ```

    or (for Windows – Replace **Python312** (Python version 3.12) with the Python version you want to use)

    ```shell
    '/C/Python312/python.exe' --version
    ```

    - If Python is not installed (i.e., you do not see any version number),
   then refer to [Instructions for Python Installation](1_instructions_for_python_installation.md) for a guide on installing multiple python versions for the labs.

2. Create and activate a **virtual environment** to keep your project
dependencies isolated from the system Python packages.

   **Part A**

   - A virtual environment is like a sandbox for Python projects.
     - It is a self-contained folder that has:
       - Its own Python interpreter (a copy of the Python executable).
       - Its own set of installed packages (separate from the global system).
     - This means that each project can have the exact tools and package
     versions it needs, without interfering with other projects or the system
     Python.

   - Importance of using a virtual environment:
     - Dependency Isolation
        - Each project has its own packages. Reduces cases of “project A broke
       because project B upgraded NumPy.”
     - Reproducibility
        - You can "freeze" your environment into a `requirements.txt` file.
       Others (or your future self) can recreate the same environment later.
     - Cleaner System
       - Keeps your global Python installation uncluttered. This avoids admin
       headaches (especially on shared machines or servers).

   - When you run `python -m venv .venv`, Python copies the interpreter into
   `.venv/`
   - It sets up special `bin/` (Linux/macOS) or `Scripts/` (Windows) folders
   with activation scripts.
   - When you activate the virtual environment (next step), your shell
   temporarily changes its PATH so that:
     - `python` points to `.venv/bin/python` (or `.venv\Scripts\python.exe`).
     - pip installs packages into `.venv/lib/...` instead of the global
     site-packages.

   - In the root of your project folder, run the following command to create
   the virtual environment. This will use the global python version.

    ```shell
    python -m venv .venv
    ```

   - If you want to use a specific python version to create your virtual environment, then use the python interpreter located in the correct path.

     - Example of creating a virtual environment that will use Python Version 3.12 in Windows OS:

      ```shell
      /C/Python312/python.exe -m venv .venv
      ```

     - Example of creating a virtual environment that will use Python Version 3.14 in Windows OS:

      ```shell
      /C/Python314/python.exe -m venv .venv
      ```

     - Example of creating a virtual environment that will use Python Version 3.12.10 in Linux:

      ```shell
      # Ensure you are in the project directory first
      pyenv local 3.12.10
      python --version
      python -m venv .venv
      ```

     - Example of creating a virtual environment that will use Python Version 3.14.3 in Linux:

      ```shell
      # Ensure you are in the project directory first
      pyenv local 3.14.3
      python --version
      python -m venv .venv
      ```

   - If you have multiple Python virtual environments, you can decide to name
   them differently instead of `.venv` to avoid confusion. For example, you can
   name them `.venv_3.12` and `.venv_3.14`.

   **Part B**

   - To activate the virtual environment, use the following commands:
       - For Windows (via Git Bash) - **NOTE: Git Bash is the preferred
     terminal for all the labs (for those using a Windows OS)**:

         ```shell
         source .venv/Scripts/activate
         ```

       - For Windows (via PowerShell):

         ```shell
         .venv\Scripts\Activate
         ```

         or

         ```shell
         .venv\Scripts\Activate.ps1
         ```

       - For Windows (via CMD):

         ```shell
         .venv\Scripts\activate.bat
         ```

       - For macOS/Linux:

         ```shell
         source .venv/bin/activate
         ```

       - Confirm that the virtual environment is active by executing the
     following. It should show the name of the virtual environment (e.g.,
     `.venv`) as part of the output.

         ```shell
         which python
         ```

   **Part C**

   - When you create and activate a virtual environment (.venv) in your
   terminal, you are telling your terminal to use that Python interpreter for the
   current session. PyCharm, however, does not automatically "see" what you did
   in the terminal. It keeps its own record of interpreters in its project
   settings.
   - That is why PyCharm may still ask you to configure one. To PyCharm, a
   .venv folder is just another directory until you explicitly say,
   “this is the interpreter I want to use.”
   - Do the following to set the Python Interpreter if you are using **PyCharm**
   as your IDE:
     - Go to File > Settings > Python > Interpreter.
     - Click Add Interpreter → Add Local Interpreter → Select Existing
     - Select Python as the Type
     - For the Python path: Browse to your `.venv/bin/python` (on **Linux/Mac**) or `.venv\Scripts\python.exe` (on **Windows**).
     - Select it and apply it.

3. Activate the virtual environment through your chosen IDE.

   **A. If using PyCharm**

   - Go to File > Settings > Python > Add Interpreter.
   - Then select "Add Local Interpreter"
   - Since the virtual environment was already created, you will see the message ".venv already exists in the specified folder".
   - Therefore, choose "Select Existing Environment".
   - In the next window, specify the path that points to the ".venv" folder inside your project directory.
   ![img.png](https://raw.githubusercontent.com/course-files/classlab/refs/heads/main/assets/images/activate_venv_pycharm.png)

   **B. If using VS Code**

     - Go to Settings > Command Palette.
     - Type "Python: Select Interpreter" and select it.
     - Choose the interpreter that points to your `.venv` folder.
     ![img.png](https://raw.githubusercontent.com/course-files/classlab/refs/heads/main/assets/images/activate_venv_vscode.png)

4. Set the correct environment by creating a `.env` file in the root of the repository. Add the values of the variables listed in [.env.example](/.env.example) as discussed in class:

5. Install the required packages depending on the environment:
    - [base.txt](../requirements/base.txt): Defines the fundamental packages that the code in the repository needs to be installed for it to run. It is: Environment-agnostic, developer-curated, stable, and minimal.
    - [dev.txt](../requirements/dev.txt): Defines what a developer needs to work productively and safely. It can include linters, formatters, test frameworks, and interactive tools. It should not include platform-specific constraints or deployment-only dependencies.
    - [colab.txt](../requirements/colab.txt): It is platform-specific for Google Colab. It specifies the adjustments required when you are running the notebook in Colab, e.g., packages that are not included in Colab by default, and compatibility pins to avoid breaking Colab.
    - [prod.txt](../requirements/prod.txt): Defines what must be installed in a production environment. It includes runtime frameworks (TensorFlow).
    - [constraints.txt](../requirements/constraints.txt): Defines the exact versions of packages to ensure compatibility and reproducibility across environments. It is used as a constraint file to pin versions when installing from dev.txt or prod.txt.
    - **Governing Rule:** If the application code imports it to run, it belongs in base.txt. If only a developer uses it to think, test, or explore, it belongs in dev.txt.

    - Run one of the following depending on your environment once the virtual environment is active (**Note:** you should be using a "development" environment to do the lab):
  
      - Production environment:

        ```shell
        pip install -r requirements/prod.txt
        ```

      - Staging environment:

        ```shell
        pip install -r requirements/dev.txt -c requirements/constraints.txt
        ```

      - Testing environment:

        ```shell
        pip install -r requirements/dev.txt -c requirements/constraints.txt
        ```

      - Colab environment:

        ```shell
        pip install -r requirements/colab.txt
        ```

      - Development environment:

        ```shell
        pip install -r requirements/dev.txt -c requirements/constraints.txt
        ```

6. You can confirm the installed packages using:

   ```shell
   pip list
   ```

---

## Project Creation Instructions

***Note:** This is for use when you are creating your own new project. You can skip this
section if you are only cloning the repository for the lab and following the
instructions to set it up.*

### Creating a Project Structure using `tree`

1. Install MSYS2 (for Windows) if not already installed.
2. Download link: [https://github.com/msys2/msys2-installer/releases](https://github.com/msys2/msys2-installer/releases)
3. Navigate to the project's root folder using the MSYS2 terminal.

```shell
tree -I ".venv|.venv_*|__pycache__|sandbox|sandbox_*|container-volumes|*.bak|lab_submission_ANSWERS"
```

## Creating a `requirements.txt` File

- The `requirements.txt` file is used for listing packages
(installable units via `pip`). Those packages usually contain the libraries you actually import and use.

**Analogy:**

- Packages → like the grocery bags you bring home from the store.
  - Example: You pip install `numpy` → you just bought a bag labeled **NumPy**.

- Libraries → like the ingredients inside those bags.
  - Example: Inside the NumPy package, you find all the mathematical functions (arrays, linear algebra, random number generators).

- When you create a `requirements.txt` file, you are making a "shopping list" of packages — the bags you must bring from the store.
- And when you write Python code, you are "cooking with the ingredients" (libraries) inside those packages.

**Option 1: Using `pipreqs`**

- `pip install pipreqs` installs the tool that scans your project code and generates a `requirements.txt` file

    ```shell
    pip install pipreqs
    ```

- `pipreqs .` looks at your imports in the source code and generates a list of basic requirements that it assumes are the most important.
- Advantage:
  - It is useful for creating a minimal `requirements.txt` file that only includes the libraries that are actually used in the code.
- Disadvantages:
  - Ignores transitive dependencies
  - Ignores runtime-only imports
  - Ignores optional and dynamic imports
  - Does not guarantee reproducibility

```shell
pipreqs . --savepath ./requirements/dev.inferred.txt --encoding=utf8 --force --ignore .venv,__pycache__,sandbox
```

**Option 2: Using `pip freeze`**

- If you want the exact versions of the libraries, you can use `pip freeze > requirements.txt` after installing the libraries in the virtual environment. This will create a `requirements.txt` file with the exact versions of the libraries installed in the virtual environment.
- Advantage:

  - It captures all installed packages in the virtual environment, including their versions.

```shell
pip freeze > ./requirements/dev.lock.txt
```
