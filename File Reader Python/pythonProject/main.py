import re
import csv
from datetime import datetime

# Define the regex pattern for matching the lines
pattern = re.compile(r'\s*data2\.addRow\(\[new Date\((\d{4}),(\d{1,2}),(\d{1,2})\),\s*(\d+\.\d+),\s*(\d+\.\d+)\]\);')


# Function to parse the lines and convert them to CSV
def convert_to_csv(input_file, output_file):
    with open(input_file, 'r') as infile, open(output_file, 'w', newline='') as outfile:
        csv_writer = csv.writer(outfile)
        csv_writer.writerow(['Date', 'Benzina', 'Motorina'])

        for line in infile:
            match = pattern.match(line)
            if match:
                year, month, day, benzina, motorina = match.groups()
                date = datetime(int(year), int(month) + 1, int(day)).strftime('%Y-%m-%d')
                csv_writer.writerow([date, benzina, motorina])


# Specify the input and output file paths
input_file = 'input_benzina_motorina.txt'
output_file = 'output.csv'

# Convert the data
convert_to_csv(input_file, output_file)
