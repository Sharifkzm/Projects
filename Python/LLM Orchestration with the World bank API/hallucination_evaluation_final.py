from sentence_transformers import CrossEncoder
import pickle

model = CrossEncoder('vectara/hallucination_evaluation_model')

# Now need to create a loop function that does this for all documents and their summaries. 
# The summaries and raw texts from the main orchestration should be located in the same folder. 
base_text_document_file_path = "text_document_{}.txt"
base_response_document_file_path = "response_document_{}.txt"
num_documents = 2  # Set the number of documents
scores_summaries_list = []


for i in range(1, num_documents + 1):
    # Construct the file paths for text and response documents
    text_document_file_path = base_text_document_file_path.format(i)
    response_document_file_path = base_response_document_file_path.format(i)

    # Read the content of the text document
    with open(text_document_file_path, "r", encoding="utf-8") as file:
        text_document_content = file.read()

    # Read the content of the response document
    with open(response_document_file_path, "r") as file:
        response_document_content = file.read()

    # Create pairs of text document and response document content
    document_summary_pairs = [[str(text_document_content),str(response_document_content)]]

    # Perform the analysis for each pair of documents
    scores_summaries = model.predict(document_summary_pairs)

    # Store or print the results
    print("Scores for document pair {}: {}".format(i, scores_summaries))
    # Great! It's giving a set of results which are showing low levels of hallucination, 0.91 and 0.81 for the two pairs

    scores_summaries_list.append(scores_summaries)

with open('scores_summaries_list.pkl', 'wb') as f:
    pickle.dump(scores_summaries_list, f)    