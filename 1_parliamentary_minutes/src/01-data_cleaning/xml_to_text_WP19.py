'''
------------------------------------------------------------------------
Script name: xml_to_text.py
Purpose of script: parliamentary minutes of WP 19 to text
Dependencies: None
Author: Alexandra Rottenkolber based on code from open-discourse
Date created: 05.03.2022
Date last modified:
------------------------------------------------------------------------
'''

import xml.etree.ElementTree as et
import os
import regex

PATH = "/Users/alexandrarottenkolber/Documents/02_Hertie_School/Master thesis/Master_Thesis_Hertie/data_analysis/01_data/Plenarprotokolle/"

# input directory
ELECTORAL_TERM_19_INPUT = PATH + "originals/pp19-data/"

# output directory
ELECTORAL_TERM_19_OUTPUT = PATH + "output/"


for xml_file in sorted(os.listdir(ELECTORAL_TERM_19_INPUT)):

    save_path = os.path.join(
        ELECTORAL_TERM_19_OUTPUT, regex.search(r"\d+", xml_file).group()
    )

    # read data
    tree = et.parse(os.path.join(ELECTORAL_TERM_19_INPUT, xml_file))
    root = tree.getroot()

    toc = et.ElementTree(root.find("vorspann"))
    session_content = et.ElementTree(root.find("sitzungsverlauf"))
    appendix = et.ElementTree(root.find("anlagen"))
    meta_data = et.ElementTree(root.find("rednerliste"))

    if not os.path.exists(save_path):
        os.makedirs(save_path)

    # save to xmls

    toc.write(
        os.path.join(save_path, "toc.xml"), encoding="UTF-8", xml_declaration=True
    )
    session_content.write(
        os.path.join(save_path, "session_content.xml"),
        encoding="UTF-8",
        xml_declaration=True,
    )
    appendix.write(
        os.path.join(save_path, "appendix.xml"), encoding="UTF-8", xml_declaration=True
    )
    meta_data.write(
        os.path.join(save_path, "meta_data.xml"),
        encoding="UTF-8",
        xml_declaration=True,
    )
