library(shiny)
library(rTRM)
library(org.Mm.eg.db)
library(org.Hs.eg.db)
library(MotifDb)
data(biogrid_mm)
data(biogrid_hs)

.getGeneReport = 
function (x, organism, sort.by = "symbol") 
{
	
	map = rTRM:::.getMapFromOrg(organism)
	dd = select(map, keys=x, columns=c("SYMBOL", "GENENAME"))

	#family = sapply(getTFclassFromEntrezgene(x), function(z) if (length(z) > 0) paste(z, collapse = " | ") else "")
	#d = data.frame(entrezgene = x, symbol = S, description = D)#, family = family, check.names = FALSE)
	d = data.frame(entrezgene = x, symbol = dd$SYMBOL, description = dd$GENENAME)#, family = family, check.names = FALSE)
	d[order(d[, sort.by]), ]
}

.getTFreport = function(organism, sort.by = "symbol") {
	e = unique(getOrthologs(organism=organism)$map_entrezgene)
	e = e[e != ""]
	.getGeneReport(e, organism, sort.by = sort.by)
}
tf_hs = .getTFreport("human")
tf_mm = .getTFreport("mouse")

tf_hs_eg=tf_hs$entrezgene
tf_mm_eg=tf_mm$entrezgene

tf_hs_name=apply(tf_hs,1,function(x) paste(x[2]," (",x[1],")",sep=""))
tf_mm_name=apply(tf_mm,1,function(x) paste(x[2]," (",x[1],")",sep=""))

names(tf_hs_eg)=tf_hs_name
names(tf_mm_eg)=tf_mm_name
 
# 
.getPPIfromOrg = function(x) {
  switch(x,
         "human" = biogrid_hs,
         "mouse" = biogrid_mm
  )}

.getFilterFromOrg = function(x) {
	map = rTRM:::.getMapFromOrg(x)
	switch(x,
		"human" = {
		 	f = c("UBC", "SUMO1", "SUMO2", "SUMO3", "SUMO4")
		},
		"mouse" = {
		 	f = c("Ubc", "Sumo1", "Sumo2", "Sumo3")
		})
	select(map,keys=f,columns="ENTREZID",keytype="SYMBOL")$ENTREZID
}

.getLayout = function(x) {
  switch(x,
         "concentric" = layout.concentric,
  			 "circle" = layout.circle,
  			 "arc" = layout.arc,
         "kamada.kawai" = layout.kamada.kawai,
         "fruchterman.reingold" = layout.fruchterman.reingold
  )
}

shinyServer(function(input, output, clientData) {
	
  motif = reactive({
    if(!is.null(input$motif$name))
      scan(input$motif$datapath, what = "")
  })
  
  query = reactive({
    if(!is.null(motif())) {
    	switch(input$query,
    		"motif" = getOrthologFromMatrix(motif(), organism=org()),
    		"motif_motifdb" = {
    			getOrthologs(values(MotifDb[motif()])$geneId,organism=org())$map_entrezgene},
    		"gene" = getOrthologs(motif(),organism=org())$map_entrezgene
    	)
    }
  })
  
  gene = reactive({
    if(!is.null(input$gene$name))
      scan(input$gene$datapath, what = "")
  })
  
  tflist = reactive({
  	switch(input$organism,
			"human" = tf_hs,
			"mouse" = tf_mm
  	)
  })
  
  tflist2 = reactive({
  	switch(input$organism,
  				 "human" = tf_hs_eg,
  				 "mouse" = tf_mm_eg
  	)
  })
  
  output$target_select=renderUI({
  	#print(head(tflist()))
  	#print(head(tflist2()))
  	selectizeInput("target","Target transcription factor",choices=tflist2(),selected=NA)
  })
    
  map = reactive({
    rTRM:::.getMapFromOrg(input$organism)
  })
  
  org = reactive({
    input$organism
  })
  
  # ABOUT TAB #####################
  output$author=renderPrint({
  	cat("Author: ", packageDescription("rTRMui")$Author, "\n")
  	cat("E-mail: ", packageDescription("rTRMui")$Maintainer, "\n")
  })
  output$trm_citation=renderPrint({
  	tmp = citation(package="rTRM")
  	cat("Title: ", tmp$title, "\n")
  	cat("Author: ", paste(tmp$author, collapse = ", "), "\n")
  	cat("Year: ", tmp$year, "\n")
  	cat("Journal: ", tmp$journal, "\n")
  })
  
  output$trmui_citation=renderPrint({
  	tmp = citation(package="rTRMui")
  	cat("Title: ", tmp$title, "\n")
  	cat("Author: ", paste(tmp$author, collapse = ", "), "\n")
  	cat("Year: ", tmp$year, "\n")
  	cat("Journal: ", tmp$journal, "\n")
  })
  
  output$biogrid=renderPrint({
  	cat("Human interactome:", "\n")
  	cat("Release: ", biogrid_hs$info$release, "\n")
  	cat("Date: ", as.character(biogrid_hs$info$date), "\n")
  	cat("Nodes: ", vcount(biogrid_hs), "\n")
  	cat("Edges: ", ecount(biogrid_hs), "\n\n")
  	cat("Mouse interactome:", "\n")
  	cat("Release: ", biogrid_mm$info$release, "\n")
  	cat("Date: ", as.character(biogrid_mm$info$date), "\n")
  	cat("Nodes: ", vcount(biogrid_mm), "\n")
  	cat("Edges: ", ecount(biogrid_mm), "\n")
  })
  
  output$package_version = renderPrint({
  	cat("rTRM:", packageDescription("rTRM")$Version, "\n")
  	cat("rTRMui:", packageDescription("rTRMui")$Version, "\n")
  	cat("igraph:", packageDescription("igraph")$Version, "\n")
  	cat("shiny:", packageDescription("shiny")$Version, "\n")
  	cat("RSQLite:", packageDescription("RSQLite")$Version, "\n")
  	cat("org.Hs.eg.db:", packageDescription("org.Hs.eg.db")$Version, "\n")
  	cat("org.Mm.eg.db:", packageDescription("org.Mm.eg.db")$Version, "\n")
  })
  #####################
  
  output$organism = reactive({
  	paste("List of", org(), "transcription factors")
  })
  
  
  output$ppi_vcount=renderText({
  	paste("PPI (original): nodes [", vcount(ppi_raw()), "] edges [", ecount(ppi_raw()))
  })
  
  output$ppi_fil_vcount=renderText({
  	paste("PPI (Ub/Sumo filtered): nodes [", vcount(ppi_fil()), "] edges [", ecount(ppi_fil()))
  })
  
  output$ppi_exp_vcount=renderText({
  	if(!is.null(ppi()))
  		paste("PPI (expressed): nodes [", vcount(ppi()), "] edges [", ecount(ppi()))
  })
  
  ppi_raw = reactive({
    .getPPIfromOrg(input$organism)
  })
  
  ppi_fil = reactive({
  	if(input$filter_ppi) {
  		f = .getFilterFromOrg(input$organism)
  		removeVertices(ppi_raw(), f)
  	} else ppi_raw()
  })
  
  ppi = reactive({
  	if(!is.null(gene())) {
  		p = ppi_fil()
  		g = gene()
  		induced.subgraph(p, V(p)[ name %in% g ])	
  	}
  })

	target = reactive({
			input$target
	})
  
  layout = reactive({
    if(!is.null(trm())) {
      layfun = .getLayout(input$layout)
      switch(input$layout,
      			 "concentric" = {
      			 		cl = getConcentricList(trm(), t=target(), e=query(),order.by=ifelse(input$sort,"label","name"))
      			 		layfun(trm(),concentric=cl, order.by=ifelse(input$sort,"label","name"))	 	
      			 },
      			 "arc" = {
      			 		layfun(trm(),target=target(),query=query())
      			 },
{
	layfun(trm())
})
    }
  })
  
  trm = reactive({
    if(!is.null(target()) && !is.null(query()) && !is.null(gene()) && !is.null(ppi())) {
      t = target()
      g = gene()
      g = unique(c(g, t)) # make sure targets are present!
      q = query()
      q = q[q %in% g]      
      p = ppi()
            
      findTRM(p, t, q, extended=input$extended, strict=input$strict, max.bridge = as.numeric(input$distance)) 
    }
    else
      NULL
  })
  

  output$trmdone = reactive({
    if(!is.null(trm()))
      TRUE # this should be a logical but there is some problem...
    else
      FALSE
  })
  
  outputOptions(output,"trmdone",suspendWhenHidden=FALSE)
  
  
  output$target = reactive({
  	if(!is.null(target()))
  		paste("Target: ", names(target()), " [", target(), "]", sep = "")
  	else
  		"Target: undefined"
  })
  
  output$debug = renderTable({
    m = ifelse(is.null(motif()), NA, length(motif()))
    q = ifelse(is.null(query()), NA, length(query()))
    e = ifelse(is.null(gene()), NA, length(gene()))
    t = ifelse(is.null(target()), NA, target())
    tn = ifelse(is.null(target()), NA, names(target()))
    p = ifelse(is.null(ppi()), NA, vcount(ppi()))
    
    data.frame(motif = m, query = q, exprs = e, target = t, name = tn, org = org(), ppi = p, extended = input$extended, strict = input$strict, dist = input$distance)
  }, include.rownames = FALSE)
  
  output$trm = renderPlot({
    if(!is.null(trm()))
      plotTRM(trm(), layout = layout(), vertex.cex = input$vsize, vertex.lwd = input$esize, edge.lwd = input$esize, label.cex = input$lsize, mar = input$margin)
  })
  
  output$legend = renderPlot({
    if(!is.null(trm()))
      plotTRMlegend(trm(), cex = 1.5)
  })
  
  output$genes = renderDataTable({
    if(!is.null(trm()))
      writeTRMreport(trm(), organism = input$organism, target = target(), query = query())
  },options=list(bFilter=FALSE))
  
  output$tfs = renderDataTable({
  	tfs = switch(input$organism,
  				 "human" = tf_hs,
  				 "mouse" = tf_mm
  	)
  	if(is.null(input$filter))
  		tfs
  	else
  		tfs[grepl(input$filter, tfs$symbol,ignore.case=TRUE), ]
  },options=list(bFilter=FALSE))
  
  output$trmtable = downloadHandler(filename = "trmtable.txt", content = function(con) { writeTRMreport(trm(), file = con, organism = input$organism, target = target(), query = query()) })
  
  output$trmlegend = downloadHandler(filename = "trmlegend.pdf",
      content = function(con) {
        pdf(file = con)
        plotTRMlegend(trm(), cex = 1.5)
        dev.off()
      }
  )

  output$trmplot = downloadHandler(filename = "trmplot.pdf",
    content = function(con) {
      if(!is.null(trm())) {
        pdf(file = con)
        plotTRM(trm(), layout = layout(), vertex.cex = input$vsize, vertex.lwd = input$esize, edge.lwd = input$esize, label.cex = input$lsize, mar = input$margin)
        dev.off()
      } else NULL
    }
  )
  
  output$targetCheck = renderUI({
  	if(!is.null(target()))
  		div(icon("check"),style="color: green;")
  	else
  		div(icon("exclamation"),style="color: darkred;")
  })
})
  
  