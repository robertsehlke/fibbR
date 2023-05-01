# fibbR - a fairly minimal GPT addin for RStudio

Or: yet another coding assistant. The package makes use of the RStudio API to stream a text response directly to the editor, based on a selected piece of code.

Why did I write this? Mostly for fun and the streaming functionality, which I hadn't seen in another native R package / RStudio addin at that point (might have changed by now).

The package includes the following functions:

- `fib_code()`: Generates an R script based on a given prompt.
- `fib_roxygen()`: Generates a roxygen docstring for a given code snippet.
- `fib()`: Sends a request to the specified model with a given prompt and optional parameters.

## Installation

```R
# Install the development version from GitHub
# install.packages("devtools")
devtools::install_github("https://github.com/robertsehlke/fibbr")
```

## Usage

### Model

Set the default model using `options(fib_model = "gpt-4")`.

`fib_model` must be either `gpt-4` (default and much recommended for R) or `gpt-3.5-turbo`.

### Authentication

On first use of one of the functions in this package you will be prompted for your OpenAI API key. This key will be saved in your system credential store using the [keyring](https://cran.r-project.org/web/packages/keyring/index.html) package.

If you need to set the token again, e.g. because it expired, you can use the `fib_set_token()` function to call up the prompt manually.

### Calling functions directly

```R
library(fibbr)

# Generate R code
fib_code("Calculate the sum of 1 to 10")

# Create a roxygen docstring for a code snippet
fib_roxygen("example_function = function(x) { return(x + 1) }")

# Send a custom request to the model
fib("What is the meaning of life?", "gpt-3.5-turbo")
```

### RStudio Addins

Addins are also available for use in RStudio:

- `fib_code_addin()`: Generates code based on the selected text in the editor.
- `fib_roxygen_addin()`: Generates a roxygen docstring for the selected code snippet.


## Other considerations

### Custom system prompts and history

Using the `fib` function directly, requests can be customised.

The function invisibly returns a nested list with the conversation history, which can be fed back in using the `history` parameter:

```
> h1 = fib("Hi there")
Hello! How can I help you?
> fib("What did I just say?", history = h1)
You said, "Hi there."
```

Adjust the system prompt:

```
> fib("Hi there", system_start_prompt = "You talk like a pirate.")
Ahoy there, matey!
```

### Streaming vs. no streaming

By default responses are streamed to the editor for `fib_code` and `fib_roxygen`. This can be controlled by setting the `stream` argument.


### Bugs and caveats

* While generating text in streaming mode, don't click elsewhere in the editor, or it will continue where you just clicked.
* Sometimes generates extra parentheses at the end (RStudio auto-closing gone rogue?).
* Works **much** better with GPT-4.
* The package is called fibbR ("fibber") because GPT-4, while much better than 3.5, will still _make shit up_ frequently - such as trying to make this package about Fibonacci.
