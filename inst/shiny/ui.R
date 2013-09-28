library(shiny)
library(rTRM)

headerPanelWithImg = function (title, windowTitle = title, src)	{
	tagList(tags$head(tags$title(windowTitle)), div(class = "span12", 
																									style = "padding: 10px 0px 0px 0px;", h1(title, style="height:80px;background-image:url(pic/logo.png);background-size:175px;background-repeat:no-repeat; padding-left:175px")))
}

shinyUI(pageWithSidebar(
	#headerPanel("rTRMui: Identification of Transcriptional Regulatory Modules"),
	headerPanelWithImg("rTRMui: Identification of Transcriptional Regulatory Modules", src = "pic/logo.png"),
							sidebarPanel(#style="width:500px",
								tags$head(
									tags$script(src="js/bootstrap.js", type="text/javascript")#,
	#								tags$script(src = "jquery.handsontable.full.js"),
	#								tags$script(src = "shiny-handsontable.js"),
	#								tags$link(rel="stylesheet", media="screen", href = "jquery.handsontable.full.css")
								),
								#h4("Example session"),
								#helpText("Download the following datasets, set 'mouse' as the target organism, set Sox2 as the target transcription factor, load the corresponding enriched motifs and expressed genes, and you are done!"),
								tags$table(tags$td(h4("Example session")), tags$td(img(src="pic/info.png",width="80%",title="Download the following datasets, set 'mouse' as the target organism, set Sox2 as the target transcription factor, load the corresponding enriched motifs and expressed genes, and you are done!"))),
								a(href="data/sox2_motifs.txt", target="_blank", "List of enriched motifs in Sox2 peaks", download="motifs"), br(),
								a(href="data/esc_expressed.txt", target="_blank", download="genes", "List of expressed genes in mouse ESC"),
								
								#tags$table(tags$td(h4("Data input")), tags$td(a(href="doc/index.html#data_input", target="help", img(src="pic/info.png",width="80%",title="Input the target organism and transcription factor, the list of query transcription factors and the list of expressed genes.")))),
								tags$table(tags$td(h4("Data input")), tags$td(img(src="pic/info.png",width="80%",title="Input the target organism and transcription factor, the list of query transcription factors and the list of expressed genes."))),
								selectInput("organism", "Target organism", choices = list("human", "mouse"), selected = NULL),
								tags$table(tags$td(textInput("target", "Target transcription factor", value = NULL)), tags$td(style="vertical-align:bottom; padding-bottom:13px; padding-left:5px", uiOutput("targetCheck"))),
								selectInput("query", "Query transcription factors", choices = list("Motif (rTRM) identifier" = "motif", "Motif (MotifDb) identifier" = "motif_motifdb", "Gene (entrezgene) identifier" = "gene")),
								fileInput("motif", "", multiple = FALSE,
													accept = NULL),
								fileInput("gene", "Expressed genes", multiple = FALSE,
													accept = NULL),
								
								#tags$table(tags$td(h4("Network parameters")), tags$td(a(href="doc/index.html#network_parameters", target="help", img(src="pic/info.png",width="80%")))),
								tags$table(tags$td(h4("Network parameters")), tags$td(img(src="pic/info.png",width="80%",title="Adjust the parameters controlling how the TRM is identified."))),
								#h4("Network parameters"),
								selectInput("extended", "Extended TRM", choices = c(TRUE, FALSE), selected = FALSE),
								selectInput("strict", "Strict TRM", choices = c(TRUE, FALSE), selected = TRUE),
								selectInput("distance", "Bridge distance", choices = 0:10, selected = 1),
								checkboxInput("filter_ppi", label="Filter Ubiquitin/Sumo from PPI", value=TRUE),
								br(),
								
								#tags$table(tags$td(h4("Plot parameters")), tags$td(a(href="doc/index.html#plot_parameters", target="help", img(src="pic/info.png",width="80%")))),
								tags$table(tags$td(h4("Plot parameters")), tags$td(img(src="pic/info.png",width="80%",title="Adjust the parameters controlling how the TRM is plotted"))),
								#h4("Plot parameters"),
								sliderInput("margin", label="Margin", min=0, max=5, value = 2),
								selectInput("layout", "Network layout", choices = c("concentric", "arc", "circle", "kamada.kawai", "fruchterman.reingold")),
								conditionalPanel(condition="input.layout=='concentric'",
																 checkboxInput("sort", label="Sort nodes by name",value=TRUE)
								),
								sliderInput("vsize", label="Node size", min=1, max=30, value = 20),
								sliderInput("esize", label="Line size", min=.5, max=10, value = 5),
								sliderInput("lsize", label="Label size", min=0.1, max=5, value = 1.5),
								br(),
								
								tags$table(tags$td(h4("Download")), tags$td(img(src="pic/info.png",width="80%",title="Download options are available once the TRM is successfully identified."))),
								#textOutput("trmdone"), # this is weird, submit bug.
								#h4("Download"), # need to use trmdone above for the condition to work with logical.
								conditionalPanel(
									condition = "output.trmdone == true",
									downloadButton("trmplot", "Plot"),
									downloadButton("trmlegend", "Legend"),
									downloadButton("trmtable", "Table")
								)
							),
							mainPanel(
								tabsetPanel(id = "tabs",
									tabPanel("Plot",
													 plotOutput("trm"),
													 plotOutput("legend")
													 #,tableOutput("debug")
									),
									tabPanel("Table",
													 tableOutput("genes")
									),
									tabPanel("Transcription factors",
													 h4(textOutput("organism")),
													 textInput("filter", label="Filter by symbol", value=NULL),
													 tableOutput("tfs")
									),
									tabPanel("Tutorial",
										h4("Identification of Sox2 TRM in ESCs:"),
										img(src="pic/tutorial.png",title="Tutorial")#,
										#h4("Manipulating TRM network parameters:"),
										#h4("Manipulating TRM plot parameters:")
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
	))