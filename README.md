# CociteR

CoCiteR is an R ShinyApp for the search of Co-Citations and Bibliographic Coupling for any given seed papers.
It uses OpenAlex to search for the citation relations.

## Usage

To run the app, clone the repository or download `app.R`. 
Then load `library(shiny)` and carry out `runApp()`. 
The ShinyApp expects a text file containing list of PubMed IDs (PMIDs) or Digital Object 
Identifiers (DOIs). See `seed_ids.txt` as an example.

This search can be time-consuming, so be patient. The more seed papers you use, 
the longer it will take. Try to stick to input numbers between two and ten seed papers.

Please enter your e-Mail address as a courtesy to OpenAlex, so they can reach out 
to you in case your requests are too extensive.


## Note

The original R script `cociteR.R` is currently able to search for Bibliographic 
Coupling, but not Co-Citations.
Its development is on hold. The current development is focused on the ShinyApp.
