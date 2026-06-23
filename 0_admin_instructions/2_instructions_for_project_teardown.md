# Cleanup Instructions

Execute the following steps once you are done with the lab,
**and you have pushed your commits to GitHub for grading**:

## Delete the Python Virtual Environment

- `.venv`

## Executing the Cleanup Scripts

***Note:** You can also wait until the end of the semester after the marks have
been officially released to implement the cleanup in all the labs.*

Clean up Part 1 of the lab:

```bash
# Ensure you are in the 1_kafka_fundamentals directory first
cd 1_kafka_fundamentals
chmod u+x project_cleanup.sh
sed -i 's/\r$//' project_cleanup.sh
./project_cleanup.sh
```

Clean up Part 2 of the lab:

```bash
# Ensure you are in the 2_containerized_microservices directory first
cd 2_containerized_microservices
chmod u+x project_cleanup.sh
sed -i 's/\r$//' project_cleanup.sh
./project_cleanup.sh
```

Clean up Part 3 of the lab:

```bash
# Ensure you are in the 3_data_engineering directory first
cd 3_data_engineering/
chmod u+x project_cleanup.sh
sed -i 's/\r$//' project_cleanup.sh
./project_cleanup.sh
```

Clean up the virtual environment for the whole repository:

```bash
# Ensure you are in the root of the repository first
chmod u+x project_cleanup.sh
sed -i 's/\r$//' project_cleanup.sh
./project_cleanup.sh
```
