# NanoDropPlotter
A tool to view multiple NanoDrop result files, no more terrible screen shots of the output!
Just save the report file from your nanodrop (.ndv), and you can import multiple files to the shiny tool to compare outputs.

The tool currently offers to ability to:
  - Merge multiple Nanodrop files together
  - Select individual samples to display
  - Save the resulting plot as a tiff image
  - Save the combined results table as a tab-delimited file
  - Save an A4 size PDF containing both plot and table together

Shiny tool beta:
https://alegione.shinyapps.io/NanoDropPlotter/



# Example:

<kbd><img src="https://github.com/alegione/NanoDropPlotter/blob/master/NanoDropPlotter/images/ExampleRun.jpeg" /></kbd>




# Current issues:
- Samples need to have unique names
- Sample order in plot and table differ (plot is alphabetical)
- Only .ndv files (that is, nanodrop report files) from an ND-1000 have been tested, if you have access to report files from other models I'd be happy to test them out and adjust the code as needed
