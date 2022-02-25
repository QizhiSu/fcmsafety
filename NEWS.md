# fcmsafety 0.1.5
## Fix the extract_meta() bug about CAS retrieval

# fcmsafety 0.1.4
## Add a function to assign meta data


# fcmsafety 0.1.2

## Enhancement

1. extract_cid() now support any of the following keys, "InChIKey", "CAS", 
or "Name", as well as their combinations. 
2. Add a default value to the cas_col argument in evaluate_compound().


## Bug fixes

1. evaluate_compound() "Error: Argument 1 must have names." error fixed.
2. Remove duplicates results if you have duplicate InChIKey values.
3. Remove warnings when load_databases().

## Others

1. Retrieval CAS number in the extract_meta() function instead of in evaluate_compound().

