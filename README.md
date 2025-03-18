# ICSE2026-Enhancing-COBOL-Code-Explanations-A-Multi-Agents-Approach-Using-Large-Language-Models
## Abstract
Common Business Oriented Language (COBOL) is a programming language used to develop business applications that are widely adopted by financial, business, and government agencies. Due to its age, complexity, and declining number of COBOL developers, maintaining COBOL codebases is becoming increasingly challenging. In particular, the lack of documentation makes it difficult for new developers to effectively understand and maintain COBOL systems. Existing research utilizes large language models (LLMs) to explain the functionality of code snippets. However, COBOL presents unique challenges due to its architectural and syntactical differences, which often cause its code to exceed the token window size of LLMs. In this work, we propose a multi-agent approach that leverages two LLM-based agents working collaboratively to generate explanations for functions, files, and the overall project. These agents incorporate together by utilizing contextual information from the codebase into the code explanation prompts. We evaluate the effectiveness of our approach using 14 open-source, real-world COBOL projects. Our results indicate that our approach performs significantly better than the baseline in function code explanation, with improvements of 12.67\%, 18.59\%, and 0.62\% in terms of METEOR, chrF, and SentenceBERT scores, respectively.  At the file level, our approach effectively explains both short and long COBOL files that exceed the token window size of LLMs and surpass the baseline by 4.21\%, 10.72\%, and 14.68\% in explaining the purpose, functionality, and clarity of the generated explanation. At the project level, our approach generates explanations that convey the functionality and purpose of 82\% of the selected projects. 

## Usage

### Dataset

### Approach

### RQ1

### RQ2

### RQ3
