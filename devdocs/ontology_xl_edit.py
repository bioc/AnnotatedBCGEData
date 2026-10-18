import os

import pandas as pd
import requests

"""
This first part of the code downloads the mapping file from Zenodo.
If the file is already on your computer, it does nothing. If changes
have been made to the mapping file recently, make sure you delete
your local copy of the mapping file before running this code, or it
will not overwrite it. 

"""

map_url = "https://zenodo.org/records/23458793/files/Metadata_Mappings_Final_10_18.xlsx?download=1"

map_filename = "Metadata_Mappings_Final_10_18.xlsx"

if not os.path.exists(map_filename):
    response = requests.get(map_url)
    response.raise_for_status()

    with open(map_filename, "wb") as file:
        file.write(response.content)


"""
This part of the code reads the mapping file into  a pandas dataframe.
The dataframe then splits the NCIT field and NCIT values column by ||,
then makes a new row for each item in the new list. This ensures there 
is only one term name per line. The code then drops the extra "comments" 
column that was created for data curation. 

The get_code function gets just the NCIT code from the NCIT field name.
The function is then used with the .apply function for the dataframe,
making a new column with just the NCIT field or value code.

"""

ont_cate = pd.read_excel(map_filename, sheet_name="Mappings")

ont_cate["Field_Name_Ontology_Terms"] = ont_cate["Field_Name_Ontology_Terms"].str.split('||', regex=False)
ont_cate = ont_cate.explode('Field_Name_Ontology_Terms').reset_index(drop=True)
ont_cate["Value_Ontology_Terms"] = ont_cate["Value_Ontology_Terms"].str.split('||', regex=False)
ont_cate = ont_cate.explode('Value_Ontology_Terms').reset_index(drop=True)
ont_cate = ont_cate.iloc[:,0:len(ont_cate.columns.tolist())-1]


def get_codes(row, col_name):
    ncit_term = row[col_name]

    ncit_list = ncit_term.split(" ")
    ncit_with_paren = ncit_list[-1]
    code_len = len(ncit_with_paren)
    code = ncit_with_paren[1:code_len-1]
    return code

ont_cate["NCIT_field_code"] = ont_cate.apply(lambda row: get_codes(row, "Field_Name_Ontology_Terms"), axis=1)
ont_cate["NCIT_value_code"] = ont_cate.apply(lambda row: get_codes(row, "Value_Ontology_Terms"), axis=1)


"""
This code writes the adjusted dataframe to a tsv file. Make sure you
upload the updated tsv file to Zenodo, and that the code pulls the 
correct version. 

"""
ont_cate.to_csv("filtered_mapped_ontology.tsv.gz", sep='\t', index=False, compression="gzip")
