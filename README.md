# Data processing on Datasets

# Group members
- Kenny Jesús Flores Huamán
- Jesús Pineda Márquez

# Description

In this work we have decided to read and process data based on selected Kaggle datasets, to later perform calculations based on statistics.

Therefore, we need to read the csv files correctly and parse the data types to be treated, being able to create a special data type in Haskell. After that, most of the functions are calculating averages, getting the most relevant elements, etc.

Next, access to the chosen Kaggle dataset will be provided:
- https://www.kaggle.com/lava18/google-play-store-apps

We must clarify that in the development of work we have optimized the two CSV that we have worked on because there were many duplicate entries, but this treatment has been done thanks to the patterns in Visual Studio Code, which has allowed us to eliminate duplicate elements or elements that They do not provide information in a very simple and fast way. Therefore, we urge you to use the CSV contained in the repository and not the ones on the website.


# Depedencies

This project uses Stack (a build tool for Haskell's projects), so the dependencies on modules are resolved when you build the project.

But, if you uses only cabal, you can use this project installing the following command:

```bash
cabal install boxes pretty # It may be necessary to add the flag "--force-reinstalls
```

# Usage

If you use Stack tool, the program can be executed using the following command from the root folder of the project:

```bash
stack run
```

