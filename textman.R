#### TEXTMAN

# Text Manipulation Functions based on Jockers 2014
# R.C. Alvarado 2017 (University of Virginia)
# Version 2


# Word Frequencies, Distributions, and Correlations within a Text

# Removes front and backmatter from a plain text file
# User passes a vector of text lines, as produced by scan(),
# and regular expressions for the beginning and end of the
# "meat" of the text. Usually these are distinctive opening
# and closing lines of text.
textman_remove_cruft = function(text.v, start_pat = '', end_pat = '') {
  text.v = text.v[-(1:grep(start_pat, text.v))] # Remove lines from first line to start_pat
  text.v = text.v[-(grep(end_pat,text.v)+1:length(text.v))] # Removed lines from end_pat to last line
  return(text.v)
}

# Converts text as vector of lines into text as vector of words. The
# resulting vector contains "clean" words -- lowercased and stripped of
# anything other than letters and spaces. The order and number of words
# is preserved; that is.
textman_lines_to_words = function(lines.v, keep_apostrophe=FALSE) {
  one.v = tolower(paste(lines.v, collapse=' ')) # Convert lines into one lowercase line
  one.v = gsub('\\s+',' ',one.v) # Collapses all space sequences into a single space
  if (keep_apostrophe == TRUE) {
    one.v = gsub("[^a-z' ]",'',one.v) # Removes anything that is not a letter or space or apostrophe
  } else {
    one.v = gsub('[^a-z ]','',one.v) # Removes anything that is not a letter or space
  }
  words.v = unlist(strsplit(one.v,'\\s+')) # Split and unlist the line into a word vector
  return(words.v)
}

# Converts text as vector of words into a contingency table using
# the table() function. The resulting table contains raw frequencies.
textman_words_to_rawfreqs = function(words.v) {
  rawfreqs.t = sort(table(words.v), decreasing = TRUE) # Use table and reverse sort results
  return(rawfreqs.t)
}

# Converts text as vector of words into a contingency table using
# the table() function. The resulting table contains relative
# frequencies, which are produced by dividing the raw values by
# the total number of words.
textman_words_to_relfreqs = function(words.v) {
  rawfreqs.t = textman_words_to_rawfreqs(words.v) # Use existing function
  relfreqs.t = 100 * (rawfreqs.t/sum(rawfreqs.t)) # Normalize results on scale of 100
  return(relfreqs.t)
}

# Returns the distribution of a word in a text, using the text
# as vector of word representation. The distribition is a vector
# of 0s and 1s of the same length as the word vector. You can think
# of it as a impression of one vector made onto another, where the
# present of the word leaves a mark.
textman_get_word_distro = function(words.v, word_str = '') {
  word_str = tolower(word_str) # Normalize string
  distro.v = rep(NA,length(words.v)) # Create empty vector size of text as words
  distro.v[which(words.v == word_str)] = 1 # Turn elements "on" where word exists
  return(distro.v)
}

# Plots the distribution of a word in a text as barcode-like graph.
textman_get_word_distro_plot = function(words.v, word_str = '', title_prefix='Dispersion Plot of', title_suffix = '') {
  distro.v = textman_get_word_distro(words.v,word_str) # Use existing function
  plot(distro.v, main=paste(title_prefix,' /',word_str,'/ ',title_suffix, sep=''),
       xlab="Novel Time", ylab="", type="h", ylim=c(0,1), yaxt='n') # Plot as barcode
  return(distro.v)
}

# Creates a list of text chunks by splitting on a break pattern,
# such as a chapter name
textman_chunk_text_by_break = function(text.v, break_pat = '') {
  chunks.l = list() # List to store each each
  breaks.v = grep(break_pat, text.v) # Get break points
  breaks.v = c(breaks.v, length(text.v)) # Add a break point to very end
  for(i in 1:length(breaks.v)) {
    if (i == length(breaks.v)) return(chunks.l) # Stop when at end
    title = text.v[breaks.v[i]] # Use the line of the break for the title
    start = breaks.v[i] + 1 # Calculate start of chunk
    end = breaks.v[i+1] - 1 # Calculate end of chunk
    chunks.l[[title]] = text.v[start:end] # Assign chunk to list
  }
}

# Get relative frequencies for each chunk in a list that was
# generated using textman_chunk_text_by_break()
textman_get_relfreqs_for_chunks = function(chunks.l) {
  my_words.l = lapply(chunks.l, textman_lines_to_words) # Existing function
  my_rfreqs.l = lapply(my_words.l, textman_words_to_relfreqs) # Existing function
  return(my_rfreqs.l)
}

# Convert list of frequency tables for chunks into a single matrix
textman_get_chunk_word_matrix = function(relfreqs.l, words.v = c()) {
  words.l = list() # List to store vectors of frequencies for each word
  for (w in words.v) {
    tmp.l = lapply(relfreqs.l,'[',w) # Get the list of frequences for the word
    tmp.m = do.call(rbind,tmp.l) # Convert list to matrix
    words.l[[w]] = as.vector(tmp.m[,1]) # Convert matrix to vector and add to master list
  }
  words.m = do.call(cbind,words.l) # Convert master list into a matrix
  colnames(words.m) = words.v # Add words as column names
  words.m[which(is.na(words.m))] = 0 # Convrt NAs to 0s
  return(words.m)
}

# Correlate the distribution of two words and test against randomized distribution
textman_get_word_pair_correlation = function(words.m, pair.v = c(1,2), n=10000) {
  word_a.v = words.m[,pair.v[1]] # Get vector for word a
  word_b.v = words.m[,pair.v[2]] # Get vector for word b
  mycor = cor(word_a.v,word_b.v) # Use the correlation function with defaults
  test.v = c() # Vector to hold test results of randomization
  for (i in 1:n) {
    test.v = c(test.v, cor(sample(word_a.v),word_b.v))
  }
  word_strs.v = colnames(words.m) # Use the words as column names
  # Package results into a list
  results = list()
  results$a     = word_strs.v[pair.v[1]]
  results$b     = word_strs.v[pair.v[2]]
  results$test  = test.v
  results$cor   = mycor
  results$min   = min(test.v)
  results$max   = max(test.v)
  results$mean  = mean(test.v)
  results$sd    = sd(test.v)
  return(results)
}

# Plot the correlation using fixed paramaters
textman_plot_word_pair_correlation = function(wcor) {
  h = hist(wcor$test,
           breaks=100,
           col="grey",
           xlab="Correlation Coefficient",
           main="Histogram of Random Correlation Coefficients\n with Normal Curve",
           plot=T)
  xfit = seq(wcor$min, wcor$max, length=1000)
  yfit = dnorm(xfit, mean=wcor$mean, sd=wcor$sd)
  yfit = yfit * diff(h$mids[1:2]) * length(wcor$test)
  lines(xfit, yfit, col="black", lwd=2)
}

# Clustering of Documents within a Collection

# Need to remove cruft from items ahead of time, which is why XML/TEI
# is a good idea -- with XML, you can select the right element. But we are
# working with plain text here.
textman_dir_to_doc_list = function(file_dir = '', file_pat = '*.txt') {
  docs.l = list() # List to hold contents of each file by filename
  files.v = dir(path=file_dir, pattern=file_pat) # Get list of filenames
  for (i in 1:length(files.v)) {
    filename = file.path(file_dir, files.v[i]) # Get full filename
    file.v = scan(filename, what = "character", sep = "\n") # Import into vector
    file.v = file.v[-grep('^\\s*#', file.v)] # Remove lines that begin with #
    docs.l[[files.v[i]]] = file.v # Add file contents to list
  }
  return(docs.l)
}

# Apply existing function to each element of the document list to convert
# from tables to dataframes
textman_get_relfreqs_for_docs = function(docs.l) {
  my_relfreqs.l = textman_get_relfreqs_for_chunks(docs.l) # Existing function
  my_relfreqs_df.l = mapply(data.frame,
                            ID=names(my_relfreqs.l),
                            my_relfreqs.l,
                            SIMPLIFY=FALSE,
                            MoreArgs=list(stringsAsFactors=FALSE)) # Convert elements of list to dataframes
  return(my_relfreqs_df.l)
}

# Convert list of frequency dataframes into a crosstab matrix
# Docs will appear as rows, words as columns
textman_get_doc_word_matrix = function(relfreqs_df.l) {
  relfreqs.df = do.call(rbind,relfreqs_df.l) # Flatten list into a dataframe
  doc_word.m  = xtabs(Freq ~ ID+words.v, data=relfreqs.df) # Create cross-tab
  return(doc_word.m)
}

# Pass the crosstab of words and docs to distance function and then
# plot results as a dendogram
# dist methods = euclidean, maximum, manhattan, canberra, binary, minkowski
textman_get_dendrogram_for_docs = function(doc_word.m, docs_relfreqs.l, method = 'euclidean', thresh = .25) {
  smaller.m = docs_words.m[,apply(doc_word.m,2,mean) >= thresh]
  dm = dist(smaller.m, method = method)
  cluster = hclust(dm)
  cluster$labels = names(docs_relfreqs.l)
  plot(cluster)
}

# Classification of Documents within a Collection

# Similar to textman_chunk_text_by_break() but by arbitrary chunk number.
# Required by textman_get_doc_chunk_freqs_list()
textman_chunk_text_by_number = function(text.v, chunk_num=400) {
  words.v = textman_lines_to_words(text.v) # Use existing function
  max_length = length(words.v)/chunk_num # Calculate the max length of a chunk
  x = seq_along(words.v) # Get a sequence equal in length to the words vectors
  chunks.l = split(words.v, ceiling(x/max_length)) # Split text into chunks
  return(chunks.l)
}

# Required by textman_get_doc_chunk_freqs_list()
# May want to use in textman_get_relfreqs_for_docs() above
textman_list_to_dataframe = function(x) {
  my.l = mapply(data.frame,
                #ID=seq_along(x), # This is used by Jockers
                ID=names(x),
                x,
                SIMPLIFY=FALSE,
                MoreArgs=list(stringsAsFactors=FALSE)
  )
  my.df = do.call(rbind, my.l)
  return(my.df)
}

textman_get_doc_chunk_word_df = function(docs.l) {
  docs_freqs.l = list()
  for (filename in names(docs.l)) {
    doc.v = docs.l[[filename]]
    doc_chunks.l = textman_chunk_text_by_number(doc.v, 10) # Existing function
    doc_chunks_freqs.l = lapply(doc_chunks.l, textman_words_to_relfreqs) # Existing function
    docs_freqs.l[[filename]] = doc_chunks_freqs.l
  }

  # Convert the list to a dataframe
  tmp.l = lapply(docs_freqs.l, textman_list_to_dataframe) # Existing function
  docs_freqs.df = do.call(rbind,tmp.l)

  # Create meaningful IDs
  doc_ids.v = gsub("\\..+$", "", rownames(docs_freqs.df))
  doc_ids.v = paste(doc_ids.v, docs_freqs.df$ID, sep="-")
  docs_freqs.df$ID = doc_ids.v

  # Convert the datafraem from long to wide form
  cross.t = xtabs(Freq ~ ID+words.v, data=docs_freqs.df)
  cross.df = as.data.frame.matrix(cross.t)

  # Add author, text, and chunk columns
  metacols.m = do.call(rbind, strsplit(rownames(cross.df), "-"))
  colnames(metacols.m) = c("author", "text", "chunk")
  cross.df = cbind(metacols.m,cross.df)

  return(cross.df)
}

textman_reduce_doc_chunk_word_df = function(wide.df, start_col=4, thresh= .5) {
  freq.means.v = colMeans(wide.df[,start_col:ncol(wide.df)])
  keepers.v = which(freq.means.v >= thresh)
  smaller.df = wide.df[, names(keepers.v)]
  smaller.df = cbind(wide.df[,c(1:start_col-1)], smaller.df) # Add back the metadata columns
  return(smaller.df)
}

textman_predict_author_with_svm = function(small.df, label = 'author', unknown = 'anonymous') {
  library(e1071)
  unknown.v = which(small.df[[label]] == unknown)     # Get vector of anonymous chunks
  train.df = small.df[-unknown.v,4:ncol(small.df)]    # Create a training set on non-anonymous chunks
  class.f = small.df[-unknown.v,label]                # Define the class column
  model.svm = svm(train.df, class.f)                  # Train the SVM model
  pred.svm = predict(model.svm, train.df)             # Test the model
  #pred.df = as.data.frame(pred.svm)                  # ditto
  confusion.m = table(pred.svm, class.f)              # Show confusion matrix (errors)
  testdata.df = small.df[unknown.v,4:ncol(small.df)]  # Create a testing set of anonymous chunks
  final_result.f = predict(model.svm, testdata.df)    # Predict using the model
  results = list()
  results$prediction = final_result.f
  results$confusion = confusion.m
  return(results)
}

# Topic Modeling of Documents within a Collection


# Similar to textman_chunk_text_by_count() but by arbitrary chunk size.
# Required by textman_get_doc_chunk_freqs_list()
textman_chunk_text_by_size = function(text.v, chunk_size=10, percentage=TRUE) {
  words.v = textman_lines_to_words(text.v, keep_apostrophe=TRUE) # Use existing function
  x = seq_along(words.v)
  if (percentage) {
    max_length = length(words.v)/chunk_size
    chunks.l = split(words.v, ceiling(x/max_length))
  } else {
    chunks.l = split(words.v, ceiling(x/chunk_size)) # deal with small chunks at the end
    if(length(chunks.l[[length(chunks.l)]]) <= length(chunks.l[[length(chunks.l)]])/2) {
      chunks.l[[length(chunks.l)-1]] = c(chunks.l[[length(chunks.l)-1]], chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]] = NULL
    }
  }
  chunks.l = lapply(chunks.l, paste, collapse=" ")
  chunks.df = do.call(rbind, chunks.l)
  return(chunks.df)
}


textman_get_doc_corpus = function(docs.l) {
  topic.m = NULL
  for (filename in names(docs.l)) {
    doc.v = docs.l[[filename]]
    doc_chunks.df = textman_chunk_text_by_size(doc.v) # Existing function
    if (!is.null(doc_chunks.df)) {
      print('# rows in doc_chunks: ')
      print(nrow(doc_chunks.df))
      print('ID: ')
      print(filename)
      docname = gsub("\\..*","", filename)
      print(docname)
      segments.m = cbind(paste(docname, segment=1:nrow(doc_chunks.df), sep="-"), doc_chunks.df)
      topic.m = rbind(topic.m, segments.m)
    }
  }
  corpus.df = as.data.frame(topic.m, stringsAsFactors=F)
  colnames(corpus.df) = c("id", "text")
  return(corpus.df)
}

textman_load_mallet = function(java_home = '/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre') {
  dyn.load(paste(java_home,'/lib/server/libjvm.dylib',sep=''))
  library(rJava)
  library(mallet)
}

textman_get_topic_model = function(corpus.df, stopwords = "stopwords.txt", z = 15, opt_interval = 40, burn_in = 80, iters = 400) {
  mallet_instances = mallet.import(corpus.df$id, corpus.df$text, stopwords, FALSE, token.regexp="[\\p{L}']+")
  topic_model = MalletLDA(num.topics=z)
  topic_model$loadDocuments(mallet_instances)
  topic_model$setAlphaOptimization(opt_interval,burn_in)
  topic_model$train(iters)
  return(topic_model)
}

textman_get_topic_model_info = function(topic_model) {
  info.l = list()
  info.l$vocabulary    = topic_model$getVocabulary()
  info.l$word_freqs    = mallet.word.freqs(topic_model)
  info.l$doc_topics    = mallet.doc.topics(topic_model,T,T)
  info.l$topic_words   = mallet.topic.words(topic_model,T,T)
  info.l$topic_labels  = mallet.topic.labels(topic_model, info.l$topic_words, 10)
  return(info.l)
}

textman_get_topic_labels = function(topic_info, nwords = 10) {
  topic_labels = list()
  for (i in 1:length(topic_info$doc_topics[1,])) {
    topic_labels[[i]] = mallet.top.words(topic_model,topic_info$topic_words[i,],nwords)
  }
  return(topic_labels)
}

#textman_print_topic_model_wordclouds(topic_model) {
#
#}
