# -*- coding: utf-8 -*-

# Overview
"""
Here are the steps the orchestration takes:
*   Step 0: Download and install packages
*   Step 1: Obtain the user's prompts and parameters
*   Step 2: Extract country and dates from the prompt
*   Step 3: Defining qterm (sector / keyword)
*   Step 4: Construct query, send API request, and dataframe the responses
*   Step 5: Pre-process the text
*   Step 6: Summarise papers and retrieve policy results
*   Step 7: Hallucination evaluation 

Initial code written with Geetika Malhotra (MPA '24), Jinyu Qi (MPA '24), Shumpei Watanbe (MPA '24).
Later revised and reworked by Sharif Kazemi (MIA '24). 
"""

# TODO: Insert your OpenAI API key into the models below
openai_api_userkey = 'INSERT_KEY_HERE'

# TODO: Code author is exploring building a user interface


## Step 0: Load packages
# Orchestration

## Loading packages
# Download relevant packages
""" In addition to Anaconda scripts, we have to install a few more packages in powershell:
pip install PyPDF2
pip install tiktoken
pip install typing-extensions==4.7
pip install -U langchain-core langchain-community langchain-openai
pip install typing-extensions==4.7 
pip install spacy
python -m spacy download en_core_web_sm
"""

# Load packages
# Sometimes we have to reinstall PyPDF2 when the env has been reset...
# !pip install PyPDF2
from PyPDF2 import PdfReader
from io import BytesIO
from langchain_core.output_parsers import StrOutputParser
from langchain_core.prompts import ChatPromptTemplate
from langchain_openai import ChatOpenAI
import nltk
from nltk.tokenize import word_tokenize
from nltk.tokenize import sent_tokenize
from nltk.tag import pos_tag
from nltk.chunk import ne_chunk
from nltk.corpus import stopwords
from sklearn.feature_extraction.text import TfidfVectorizer
from dateutil import parser
from dateutil.parser import parse
from sentence_transformers import CrossEncoder
# from google.colab import userdata # This is used when we are using Google Collab
import json
import requests
import re
import pandas as pd
import string
import tiktoken
import spacy
import textwrap

# Downlaod relevant nltk resources
# nltk.download('stopwords')
# nltk.download('punkt')
# nltk.download('maxent_ne_chunker')
# nltk.download('words')
# nltk.download('averaged_perceptron_tagger')



## Step 1: User's prompts and parameters
# This step can be done through the R portal as an input. To input through this script, amend the code below

user_prompt = "I want to know more about the education policies of India between 2010 and 2020"
# user_prompt = input("Please request your policy summary, specifying country, year, and sector of interest: ")
print("User prompt is: ", user_prompt)

documents_to_index = 5
# documents_to_index = input("Please request number of documents to index (5 is good for a short query): ")
print("Number of documents to index: ", documents_to_index)

page_limit_per_document = 30
# page_limit_per_document = input("Please request the page limit for indexed documents that will be sent to the OpenAI API (50 is good for a short query): ")
print("Number of pages per indexed document that will be sent to the OpenAI API: ", page_limit_per_document)



## Step 2: Extract country and dates from the prompt
# Country extraction (using nemed entity recognition (NER))
# Can also extract region (such as Southern Africa)

tokens = word_tokenize(user_prompt)
tags = pos_tag(tokens)
entities = ne_chunk(tags)

country = ' '.join([leaf[0] for tree in entities if hasattr(tree, 'label') and tree.label() == 'GPE' for leaf in tree.leaves()])
#country = ' '.join([' '.join(leaf[0] for leaf in tree.leaves()) for tree in entities if hasattr(tree, 'label') and tree.label() == 'GPE'])
countries_with_more_than_two_words = ["Costa Rica", "Saudi Arabia", "South Africa", "El Salvador", "Sierra Leone", "Papua New Guinea", "Sri Lanka", "Venezuela, RB", "Iran, Islamic Rep.", "United Kingdom", "United States"] # should add more

for country2 in countries_with_more_than_two_words:
    if country2 in user_prompt:
        country = country2


year_pattern = r'\b(?:19|20)(\d{2})\b'
years = re.findall(year_pattern, user_prompt)

year1 = None
year2 = None
if len(years) > 1:     # This algorithm works until 2029! It works if the year range is between 1930-2029
# TODO: Investigate if we can set the year-range more dynamically than this
    if int(years[0]) < 30:
        year1 = "20" + years[0]
    else:
        year1 = "19" + years[0]

    if int(years[1]) < 30:
        year2 = "20" + years[1]
    else:
        year2 = "19" + years[1]
elif len(years) == 1:
    if int(years[0]) < 30:
        year1 = "20" + years[0]
        year2 = "20" + years[0]
    else:
        year1 = "19" + years[0]
        year2 = "19" + years[0]
else:
    print("Could not find a valid year")

print(country, year1, year2)



## Step 3: Defining qterm (sector / keyword) for World Bank API
nlp = spacy.load("en_core_web_sm")

def preprocess(user_prompt):
    # Lowercase, remove punctuation, and tokenize the text.
    user_prompt = user_prompt.lower()
    user_prompt = re.sub(r'[^\w\s]', '', user_prompt)
    tokens = word_tokenize(user_prompt)
    stop_words = set(stopwords.words('english'))
    tokens = [word for word in tokens if word not in stop_words]
    return tokens


country_lower = country.lower()

def extract_sector_keywords(user_prompt):
    # Process the user prompt using spaCy
    prompt = nlp(user_prompt)

    # Initialize an empty set to store extracted keywords
    extracted_keywords = set()

    # Extract nouns and noun phrases as potential sector keywords
    for token in prompt:
        if token.pos_ in ["NOUN", "PROPN"]:
            keyword = token.text.lower()            
            if keyword not in ['policy', 'policies', country_lower]:  # Exclude 'policy' and 'policies' from keywords
                extracted_keywords.add(keyword)
        elif token.pos_ == "ADJ" and token.head.pos_ in ["NOUN", "PROPN"]:
            # Including adjectives that modify nouns or noun phrases
            extracted_keywords.add(token.text.lower())

    return extracted_keywords

def identify_sector(user_prompt, sector_keywords):
    # Identify the most likely sector by matching keywords, returning only the top sector
    tokens = preprocess(user_prompt)
    sector_counts = {sector: 0 for sector in sector_keywords}

    # Count matches for each sector
    for token in tokens:
        for sector, keywords in sector_keywords.items():
            if token in keywords:
                sector_counts[sector] += 1

    # Find the sector with the highest count
    most_likely_sector = max(sector_counts, key=sector_counts.get)

    # Return the sector with the highest count, considering None for no matches
    return most_likely_sector if sector_counts[most_likely_sector] > 0 else None


extracted_keywords = extract_sector_keywords(user_prompt)

# Define sector keywords dictionary based on extracted keywords
sector_keywords = {keyword: extracted_keywords for keyword in extracted_keywords}

# Identify the sector from the extracted keywords
most_likely_sector = identify_sector(user_prompt, sector_keywords)

print("Most Likely Sector:", most_likely_sector)


## Step 4: Construct query, send API request to World Bank, and dataframe response
if country and years:
    url = f'https://search.worldbank.org/api/v2/wds?format=json&rows={documents_to_index}&count_exact={country}&qterm={most_likely_sector}&strdate={year1}-01-01&enddate={year2}-12-31'
    response = requests.get(url)
    if response.ok:
         data = response.json()
    else:
         print("Error with the API request")
else:
    print("Could not find a valid country or date range in the prompt")


if 'documents' in data:
    documents = data['documents']
else:
    documents = data


research_index_csv = pd.DataFrame(documents)
research_index_csv.to_csv('research_index.csv', index=True)
try:
    url_link_row = research_index_csv.loc["pdfurl"]
except Exception as e:
    print("An error occurred:", e)
    print("No documents were found in the WB Database which match the specified year, country, and sector. Please broaden your search parameters and try again.")
    raise

# If we want to download the csv from the API in Google Collab:
# from google.colab import files
# files.download('df.csv')



## Step 5: Pre-process the text from World Bank documents received
research_index_csv = pd.read_csv('research_index.csv')

metadata_full_text = []

# New function for reducing token load
def clean_text(text):
    paragraphs = text.split('\n')
    clean_paragraphs = []
    for paragraph in paragraphs:
        if not paragraph.strip():
            continue
        if any(keyword in paragraph.lower() for keyword in ['table', 'figure', 'chart', 'appendix']):
            continue
        clean_paragraphs.append(paragraph)
    return '\n'.join(clean_paragraphs)


"""
# TODO: Function for extracting text indicative of an abstract
def extract_abstract(text):
    sentences = sent_tokenize(text)
    abstract_sentences = []

    # Keywords indicating the start of an abstract
    abstract_keywords = ["abstract", "summary", "introduction"]

    for sentence in sentences:
        # Check if the sentence contains any abstract keywords
        if any(keyword in sentence.lower() for keyword in abstract_keywords):
            abstract_sentences.append(sentence)
        # Break if a blank line is encountered, indicating the end of the abstract
        elif not sentence.strip():
            break

    # Return the extracted abstract text
    return ' '.join(abstract_sentences)
"""

# Function for exporting individual documents
def export_document(text, document_number):
    file_name = f"text_document_{document_number}.txt"
    with open(file_name, "w", encoding="utf-8") as file:
        file.write(text)
    print(f"Document {document_number} exported successfully as {file_name}")

full_text_list = [] 
full_text2 = []
total_tokens = 0 # for length of saved documents to meet token limit of API
document_count = 0
tokens = 0

# Looping through all the pdflinks available in .csv
for i, url_link in enumerate(url_link_row.values):

    print(f"'URL' number {i+1} is -  {url_link}")
    if pd.isna(url_link):
      continue

    response = requests.get(url_link) #To fetch the pdf from url dynamically

    with BytesIO(response.content) as data:
        reader = PdfReader(data)
        abstract = ""
        for page_num, page in enumerate(reader.pages, start=1):
            full_text_list.append(page.extract_text())     

        if len(reader.pages) > int(page_limit_per_document):
            print(f"PDF has more than {page_limit_per_document} pages, skipping...")
            continue # One workaround for token limit issue

    for page in full_text_list[:1]:
        title_prompt = page[:200] + '...' + '\n'

    print(f"Title is - '{title_prompt}'. ")

    full_text = f"Document {i+1}:\n" + ' '.join(full_text_list) + '\n""""""""""'  ###adding a delimiter and document number
    print('original pdf length: ', len(full_text))
    # Split to get rid of references
    full_text = full_text.split('REFERENCES')[0]
    print('After taking out references', len(full_text))

    cleaned_text = clean_text(full_text)
    print('After cleaning:', len(cleaned_text)) # TODO: Length is doing funny things where it's

    # Count # of tokens in document
    # This helps us to understand if we're reaching the OpenAI token limit
    encoding = tiktoken.encoding_for_model("gpt-3.5-turbo")
    tokens = encoding.encode(cleaned_text)

    if (total_tokens + len(tokens)) > 120000: # 16385 for GPT 3.5 (15000 to be safe), 128000 for GPT 4 (120000 to be safe)
        print(len(tokens), "tokens. Skipping document due to token limit.")
        continue
    # If the document can be added without exceeding the token limit for the OpenAI API, append it to full_text2

    export_document(cleaned_text, i+1)  # Call a function to export the document

    metadata_full_text.append(cleaned_text)
    full_text2.append(cleaned_text)

    # Increment document counter for future evaluation 
    document_count += 1

    # Update the total token count
    total_tokens += len(tokens)

# Script exit command if document_count is zero
def check_document_count(document_count):
    if document_count == 0:
        print("Document count is zero. Stopping execution. Recommend expanding number of documents to index and/or pages per document, and then re-running the script.")
        return False
    else:
        return True


if not check_document_count(document_count):
    # If check_document_count returns False, stop execution and print error listed above.
    exit() 


# If document count is greater than zero, then we can have a look at the full appended text and number of documents.
print("Number of documents for summary:", document_count)
print(full_text2)



# Export function for all documents
def export_text_to_file(full_text2, filename):
    with open(filename, 'w', encoding='utf-8') as file:
        for text in full_text2:
            file.write(text + '\n')

export_text_to_file(full_text2, 'exported_text.txt')
# Exports now fixed


if isinstance(full_text2, list):
    # Convert list to string
    full_text2 = ''.join(full_text2)

documents = full_text2.split('""""""""""') 


# TODO: WORKS AT A BASIC LEVEL, BUT NEED TO SKIP DOCUMENTS THAT WEREN'T SENT TO THE API AND ALSO EXTRACT ABSTRACT OF PAPERS AT AN EARLIER STAGE
## Step 6A: Using OpenAI's API, summarise a single paper indexed, retrieve policy results, and export the text
# Summarise only one of the documents
for document_number, document_text in enumerate(documents, start=1):
    if document_number > document_count:
        break # Stopping a hallucinated summary of a phantom document at the end      
    prompt = ChatPromptTemplate.from_template(f"Please summarise the suggested policies regarding {most_likely_sector} about {country} in {document_text}. Please reference the sources in {document_text} as much as possible. Use clear and accessible language which can be easily understood by a non-native English speaker or a high-school student")
    model = ChatOpenAI(model="gpt-4-turbo", openai_api_key='INSERT_API_KEY_HERE')
    output_parser = StrOutputParser()

    chain = prompt | model | output_parser
    response_single_document = chain.invoke({
        "document_i_text": document_text,
        "country": country,
        "most_likely_sector": most_likely_sector
    })

    print(response_single_document)

    with open(f"response_document_{document_number}.txt", "w") as file:
        file.write(response_single_document)

    print(f"Individual document summary has been exported to response_{document_number}.txt")




## Step 6B: Using OpenAI's API, summarise all papers, retrieve policy results, and export the text
# Summarise entire documents
prompt = ChatPromptTemplate.from_template(
  "Please summarise the suggested policies regarding {most_likely_sector} about {country}. Please reference the sources in {full_text} as much as possible and use the following format: 'Document number, title of paper, key policies insights. Please provide a summary of all documents at the end of your response. Use clear and accessible language which can be easily understood by a non-native English speaker or a high-school student.")
model = ChatOpenAI(model="gpt-4-turbo",
                  openai_api_key='INSERT_API_KEY_HERE')
output_parser = StrOutputParser()

chain = prompt | model | output_parser
try:
    response_all_documents = chain.invoke({
        "full_text": full_text2,
        "country" : country,
        "most_likely_sector" : most_likely_sector})
except Exception as e:
    print("Error occurred:", e)
    print("\n")
    print("If the error refers to rate limit, then the request exceeds the token limit of OpenAI's API. Please try again by selecting fewer documents to index, reducing the page limit for indexed documents, and/or referencing other parameters to retrieve smaller documents")
    raise


# Let's create a function that prints the LLM responses in a tidier way
def print_wrapped(text, width=80):
    wrapped_text = textwrap.fill(text, width=width)
    print(wrapped_text)

print_wrapped(response_all_documents, width=80)

# Now we can export our summary for further analysis in R
with open("response_all_documents.txt", "w") as file:
    file.write(response_all_documents)  

print("Policy summary has been exported to response_all_documents.txt")

print("Number of documents that have been summarized:", document_count)

