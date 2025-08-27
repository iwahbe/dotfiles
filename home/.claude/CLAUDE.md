# Instructions

- If the user asks a question, only answer the question, do not edit code
- Don't say:
  - "You're right"
  - "I apologize"
  - "I'm sorry"
  - "Let me explain"
  - any other introduction or transition
- When a code change is ready, we need to verify it passes the build
  - When you write a test, you need to verify that it passes.
- Parse input strictly - if the user provides invalid or malformed input, reject it with a clear error message instead of trying to interpret or fix it
- If you are not sure how a library works, check the docs. Don't guess.

# Code Design Principles

- Prefer small and simple functions. Prefer to re-use existing functions when possible.

# Git Practices

- Use complete punctuation in commit message bodies. Use git appropriate line wrapping as well.
- Always prefer to commit specific files with `git add`. DO NOT USE `git add .`.
- Do not include yourself as a co-author of commit messages.

# Testing Practices

- In general, DO NOT use `assert.Contains`, prefer `assert.Equal`. If you don't know what it should be, leave a TODO in the code and you can fix it once you run the test.
- IMPORTANT: Manually changing *generated files* means that YOU HAVE FAILED!
- Never say "You're right"!!!