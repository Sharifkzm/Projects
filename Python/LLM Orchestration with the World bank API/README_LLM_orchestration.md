**LLM Orchestration with the World Bank API**

**IMPORTANT:** To run the master script, please open 'llms_orchestration_master_final' and insert your OpenAI API key under the variable *openai_api_userkey* on line 20.

Once the script runs, the user is asked to input three variables into the model:
1. user_prompt: Please request your policy summary, specifying country, year, and sector of interest
2. documents_to_index: Please request number of documents to index (5 is good for a short query)
3. page_limit_per_document: Please request the page limit for indexed documents that will be sent to the OpenAI API (50 is good for a short query)

Example prompts are listed in the script. 

Once the master script has run and created the outputs (at least one document), then the hallucination evaluation script can be run.

