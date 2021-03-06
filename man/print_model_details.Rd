% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_markdown.R
\name{print_model_details}
\alias{print_model_details}
\title{Convenience method to output a model object as markdown.}
\usage{
print_model_details(model, title = "**Model details**", caption = "",
  output_file)
}
\arguments{
\item{model}{The model object to print.}

\item{title}{The title for the table.}

\item{caption}{The caption for the table.}

\item{output_file}{(optional) The file to which to write the markdown.}
}
\description{
Markdown is emitted to the console, and optionally to an external file.
}
\details{
This function takes as input a model object and applies \code{\link[broom:tidy]{broom::tidy()}} to
it to obtain a data frame. See the documentation and vignettes of broom
for more detail.
}
\examples{
## Annette Dobson (1990) 'An Introduction to Generalized Linear Models'.
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c('Ctl','Trt'))
weight <- c(ctl, trt)
test_lm <- lm(weight ~ group)

print_model_details(model = test_lm,
                    title = '**model markdown**',
                    caption = 'example using plant weight data')
}
\seealso{
\code{\link[broom:tidy]{broom::tidy()}}, \code{browseVignettes('broom')}
}
