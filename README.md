# ASE2025-Enhancing-COBOL-Code-Explanations-A-Multi-LLMs-Approach
## Abstract
Common Business Oriented Language (COBOL) is a programming language used to develop business applications that are widely adopted by financial, business, and government agencies. Due to its age, complexity, and declining number of COBOL developers, maintaining COBOL codebases is becoming increasingly challenging. In particular, the lack of documentation makes it difficult for new developers to effectively understand and maintain COBOL systems. Existing research utilizes large language models (LLMs) to explain the functionality of code snippets. However, COBOL presents unique challenges due to its architectural and syntactical differences, which often cause its code to exceed the token window size of LLMs. In this work, we propose a multi-agent approach that leverages two LLM-based agents working collaboratively to generate explanations for functions, files, and the overall project. These agents incorporate together by utilizing contextual information from the codebase into the code explanation prompts. We evaluate the effectiveness of our approach using 14 open-source, real-world COBOL projects. Our results indicate that our approach performs significantly better than the baseline in function code explanation, with improvements of 12.67\%, 18.59\%, and 0.62\% in terms of METEOR, chrF, and SentenceBERT scores, respectively.  At the file level, our approach effectively explains both short and long COBOL files that exceed the token window size of LLMs and surpass the baseline by 4.21\%, 10.72\%, and 14.68\% in explaining the purpose, functionality, and clarity of the generated explanation. At the project level, our approach generates explanations that convey the functionality and purpose of 82\% of the selected projects. 

![Overview of our multi-agent approach](figures/cobol_appraoch.PNG)

## Folder Descriptions

- `COBOL_basic_definition/`: Contains a synthetic COBOL example designed to help readers unfamiliar with COBOL understand the basic program structure.
- `Manual Evaluation/`: Includes manual evaluation results for all three research questions (RQ1–RQ3), such as scores for reference coverage, hallucination detection, business relevance, functionality, and completeness.
- `project_statistic_info/`: Provides statistics for the studied open-source COBOL projects, including lines of code (LOC), token counts, and project descriptions.
- `Prompts/`: Contains all the prompt templates used for code, file, and project-level explanation generation in the paper.
- `usefulness_analysis/`: Includes qualitative case studies 
    - `usefulness_analysis.xlsx` shows both good and bad examples across different levels, demonstrating the usefulness of our approach and reason.
    - `response_purpose_compare_explanation.csv`: Contains analysis that explains why we adopt a bottom-up strategy and focus on file-level purpose, showing how it leads to more consistent and accurate explanations.
    - `function_level_example_text_processing.png`: Provides function-level examples illustrating the benefits of incorporating a Text Processing LLM and the motivation for using it in generating refined summaries.

## Usage
1. Run ```pip install -r requirements.txt``` to install all the requirements.
2. Execute the notebooks in ```script``` sequentially to reproduce our approach and experiment results.

### Dataset
```data/projects``` provides the raw data of the 14 COBOL projects.

```data/function_level_reference_dataset.csv``` provides the dataset of COBOL paragraphs with corresponding comments as reference data. The comments are in the 'reference data' column.

```data/file_level_reference_dataset.csv``` provides the dataset of COBOL files with corresponding head comments as reference data. The comments are in the 'reference data' column.

### Approach
```script/extract_artifact.ipynb``` provides the code for extracting COBOL artifacts at function, file, and project level. Install all the requirements and run the notebook sequentially. The extracted artifacts can be found in ```data/function_refined_summary.csv``` and ```data/file_generated_summary.csv``` respectively.

### RQ1
```data/function_level_reference_dataset.csv``` provides the explanation generated by our approach by Code Processing Agent, Text Processing Agent, and the baseline, which can be found in 'granite 34b', 'refined summary2' and 'baseline' column respectively.

```Manual Evaluation/function_generated_manual_labeled.csv``` provides the result for manually evaluating generated function explanations. The manual evaluation scores are in the 'reference coverage' and 'hallucination detection' column.

```Prompts/Function-Level Prompt Code Processing Agent``` and ``Prompts/Function-Level Prompt Text Processing Agent``` are function level generation prompts

### RQ2
```data/file_level_reference_dataset.csv``` provides the explanation generated by our approach and the baseline, which can be found in 'refined summary2' and 'baseline' column respectively.

```Manual Evaluation/long_file_generated_manual_labeled.csv``` provides the result for manually evaluating generated long file explanations. The manual evaluation scores are in the 'business', 'functionality', 'completeness' columns.

```Manual Evaluation/short_file_generated_manual_labeled.csv``` provides the result for manually evaluating generated short file explanations. The manual evaluation scores are in the 'business', 'functionality', 'completeness' columns.

```Prompts/File-Level Short File Code Processing Agent``` and ```Prompts/File-Level Long File Text Processing Agent``` are file level generation prompts

### RQ3
```data/project_generated_summary.csv``` provides the project-level explanation generated by our approach, and the explanation can be found in 'generated summary column.

```Prompts/Project-Level Prompt Text Processing Agent``` is project level generation prompts

### LLM-as-a-Judge
```llm judge``` contains all three level's llm judge results and script
