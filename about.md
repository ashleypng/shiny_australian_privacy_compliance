
# Australian Privacy Compliance Tool

This Shiny application helps identify and redact sensitive personal information in data files according to:

1. **Australian Privacy Principles (APPs)** from the Privacy Act 1988 (Cth)
2. **Information Privacy Principles (IPPs)** from the Victorian Privacy and Data Protection Act 2014

## Key Privacy Principles

### Australian Privacy Principles (APPs)

- **APP 3: Collection of solicited personal information**
  - Only collect personal information that is reasonably necessary
  - Get consent to collect sensitive information

- **APP 6: Use or disclosure of personal information**
  - Only use or disclose personal information for the primary purpose it was collected
  - Secondary use requires consent or specific exemptions

- **APP 9: Adoption, use or disclosure of government related identifiers**
  - Organizations should not adopt, use or disclose a government related identifier

- **APP 11: Security of personal information**
  - Take reasonable steps to protect personal information from misuse, interference, loss, unauthorized access, modification or disclosure

### Victorian Information Privacy Principles (IPPs)

- **IPP 10: Sensitive Information**
  - An organization must not collect sensitive information about an individual unless specific conditions are met

## What This Tool Does

1. Analyzes uploaded CSV and Excel files
2. Identifies potentially sensitive fields based on column names
3. Provides explanation of which privacy principles apply
4. Enables creation of redacted versions of the data

## Limitations

This tool performs basic analysis based on column names and common patterns. It should be used as an aid only and not as definitive legal advice. For comprehensive privacy compliance, consult with a privacy professional.

## References

- [Office of the Australian Information Commissioner (OAIC)](https://www.oaic.gov.au/)
- [Office of the Victorian Information Commissioner (OVIC)](https://ovic.vic.gov.au/)

