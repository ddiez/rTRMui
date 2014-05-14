library(shiny)
library(rTRM)

shinyUI(
	#pageWithSidebar(
	fluidPage(
	titlePanel(
		div("rTRMui: Identification of Transcriptional Regulatory Modules", style="height:80px;background-image:url(pic/logo.png);background-size:175px;background-repeat:no-repeat; padding-left:175px"),"rTRMui"),
	sidebarLayout(
	sidebarPanel(style="width:250px;",
							 # Example session.
							 withTags(table(style="width: 100%",td(h4("Example session")),td(style="text-align: right;color: grey;", icon("info-circle","fa-lg"),title="Download the following datasets, set 'mouse' as the target organism, set Sox2 as the target transcription factor, load the corresponding enriched motifs and expressed genes, and you are done!"))),
							 em("Sox2 dataset on ESCs:"),
							 withTags(table(style="width: 100%",tr(td("List of enriched motifs"),td(style="text-align: right;padding-right:3px;", a(icon("download"),href="data/sox2_motifs.txt", target="_blank", download="motifs"))),tr(td("List of expressed genes"),td(style="text-align: right;padding-right:3px;", a(icon("download"),download="genes",href="data/esc_expressed.txt",target="_blank"))))),
							 
							 # Data input.
							 withTags(table(style="width: 100%",td(h4("Data input")), td(style="text-align: right;color: grey;", icon("info-circle","fa-lg"),title="Input the target organism and transcription factor, the list of query transcription factors and the list of expressed genes."))),
							 selectInput("organism", "Target organism", choices = list("human", "mouse"), selected = NULL),
							 uiOutput("target_select"),
							 selectInput("query", "Query transcription factors", choices = list("Motif (rTRM) identifier" = "motif", "Motif (MotifDb) identifier" = "motif_motifdb", "Gene (entrezgene) identifier" = "gene")),
							 fileInput("motif", "", multiple = FALSE, accept = NULL),
							 fileInput("gene", "Expressed genes", multiple = FALSE, accept = NULL),
							 
							 # Network parameters.
							 withTags(table(style="width: 100%", td(h4("Network parameters")), td(style="text-align: right;color: grey;", icon("info-circle","fa-lg"), title="Adjust the parameters controlling how the TRM is identified."))),
							 selectInput("extended", "Extended TRM", choices = c(TRUE, FALSE), selected = FALSE),
							 selectInput("strict", "Strict TRM", choices = c(TRUE, FALSE), selected = TRUE),
							 selectInput("distance", "Bridge distance", choices = 0:10, selected = 1),
							 checkboxInput("filter_ppi", label="Filter Ubiquitin/Sumo from PPI", value=TRUE),
							 br(),
							 
							 # Plot parameters.
							 withTags(table(style="width:100%;",td(h4("Plot parameters")), td(style="text-align: right;color: grey;", icon("info-circle","fa-lg"),title="Adjust the parameters controlling how the TRM is plotted"))),
							 sliderInput("margin", label="Margin", min=0, max=5, value = 2),
							 selectInput("layout", "Network layout", choices = c("concentric", "arc", "circle", "kamada.kawai", "fruchterman.reingold")),
							 conditionalPanel(condition="input.layout=='concentric'",
							 								 checkboxInput("sort", label="Sort nodes by name",value=TRUE)
							 ),
							 sliderInput("vsize", label="Node size", min=1, max=30, value = 20, step=1),
							 sliderInput("esize", label="Line size", min=.5, max=10, value = 5,step=0.5),
							 sliderInput("lsize", label="Label size", min=0, max=5, value = 1.5,step=0.5),
							 br(),
							 
							 # Download.
							 withTags(table(style="width:100%;",td(h4("Download")), td(style="text-align: right;color: grey;", icon("info-circle","fa-lg"),title="Download options are available once the TRM is successfully identified."))),
							 conditionalPanel(
							 	condition = "output.trmdone == true",
							 	downloadButton("trmplot", "TRM"),
							 	downloadButton("trmlegend", "Legend"),
							 	downloadButton("trmtable", "Table")
							 )
	),
	mainPanel(style="left:300px;width:600px;",
		tabsetPanel(id = "tabs",
								tabPanel("Plot",
												 plotOutput("trm"),
												 plotOutput("legend")
								),
								tabPanel("Table",
												 dataTableOutput("genes")
								),
								tabPanel("Transcription factors",
												 h4(textOutput("organism")),
												 em("This list includes all TFs for which there is a PWM in the rTRM database. The target organism matches the one selected in the left panel."),br(),br(),
												 textInput("filter", label="Filter by symbol", value=NULL),
												 dataTableOutput("tfs")
								),
								tabPanel("Tutorial",
												 #h4("Identification of Sox2 TRM in ESCs:"),
												 #img(src="pic/tutorial.png",title="Tutorial")
												 includeHTML(system.file(package="rTRMui","shiny/www/doc/tutorial.html"))
								),
								tabPanel("Help",
												 includeHTML(system.file(package="rTRMui","shiny/www/doc/index.html"))
								),
								tabPanel("About",
												 h4("Contact"),
												 verbatimTextOutput("author"),
												 a(href="http://www.ifrec.osaka-u.ac.jp/en/laboratory/qiru/index.php", target="_blank", "Quantitative Immunology Research Unit (IFReC)"),
												 h4("Citation"),
												 p("To cite rTRMui please use:", br(), verbatimTextOutput("trmui_citation")),
												 p("To cite rTRM please use:", br(), verbatimTextOutput("trm_citation")),
												 h4("Package version"),
												 verbatimTextOutput("package_version"),
												 a(href="http://www.bioconductor.org", target="_blank", "Bioconductor web site"), br(),
												 a(href="http://tfclass.bioinf.med.uni-goettingen.de/tfclass", target="_blank", "TFClass web site"),
												 h4("BioGRID data"),
												 verbatimTextOutput("biogrid"),
												 a(href="http://www.thebiogrid.org", target="_blank", "The BioGRID web site")
								)
		)
	)
)
))