# Instructions

- If the user asks a question, only answer the question, do not edit code
- **NEVER SAY**:
  - "You're right"
  - "I apologize"
  - "I'm sorry"
  - "Let me explain"
  - any other introduction or transition
- When a code change is ready, we need to verify it passes the build
  - When you write a test, you need to verify that it passes by running the test.
- Parse input strictly - if the user provides invalid or malformed input, reject it with a clear error message instead of trying to interpret or fix it
- If you are not sure how a library works, check the docs. **Don't guess!**

# Code Design Principles

- Prefer small and simple functions. Prefer to re-use existing functions when possible.

# Git Practices

- Use complete punctuation in commit message bodies. Use git appropriate line wrapping as well.
- Always prefer to commit specific files with `git add`. DO NOT USE `git add .`.
- Do not include yourself as a co-author of commit messages.

# Testing Practices

- IMPORTANT: Manually changing *generated files* means that YOU HAVE FAILED!

## Golang

- In general, DO NOT use `assert.Contains`, prefer `assert.Equal`. If you don't know what it should be, leave a TODO in the code and you can fix it once you run the test.
- If you want to check that a struct matches another struct, use `assert.Equal` on the whole struct, don't assert on each field individually:

		assert.Equal(t, SomeStruct{
	    	Field1: "string",
            Field2: false,
        }, actualValue)
    
    The above should be preferred over this:

        assert.Equal(t, "string", actualValue.Field1)
        assert.Equal(t, false, actualValue.Field2)

- When writing tests for Go, prefer to assert on a whole struct rather then asserting on each field individually.
- After you have edited code, please remove any comments that don't convey more information then the line of code that's attached. Comments should always convey information that isn't in the code, not just restate what is in the code.