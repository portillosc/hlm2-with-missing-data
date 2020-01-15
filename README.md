# Download *HLM2 with Missing Data* to run on your computer without accessing the website.
### Minimum requirements to run this program on your computer (links are for the sources)
* [R-3.6.1 or newer](https://cran.r-project.org/)
* [R-studio Desktop](https://rstudio.com/) (the normal version) 
* Updated versions of the R packages 'shiny' and 'foreign'

### Quickstart
1. Click the arrow on the green "Clone or download" button on the top of this page
2. Select "Download ZIP"
3. Unzip the folder and open the file "hlm_refactor_2.Rproj" - it should automatically open R-studio
4. Within R-studio, navigate to the "Files" tab in the bottom right window
5. Open server.R
6. If you are unsure whether the R packages "shiny" and "foreign" are updated, continue reading this step. Navigate to the "Console" tab on the bottom left window of the app, click to the right of the ">"  and see a blinking "|", then enter the following commands:

```
install.packages('shiny')
```

7. In the server.R window, click the "Run App" button in the top left corner.

After running the app, there will be many intermediate files produced. Do not be alarmed, these are necessary for the application to run. These files are overwritten every run, so there is no concern of consuming too much memory. You may delete these intermediate files after you complete your work with this application.
