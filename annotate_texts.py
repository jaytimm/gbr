

import os
import pandas as pd

import nltk
nltk.download('punkt')
import spacy
nlp = spacy.load("en_core_web_sm")
nlp.disable_pipes('ner', 'parser')
nlp.add_pipe("sentencizer", first=True)

import time
import re
import string

from tqdm import tqdm
yas = os.listdir(out)
yas0 = [re.sub("_.*$", '', file) for file in yas]

txts =  os.listdir('/home/jtimm/gutenberg/data/text/')
#txts = list(filter(lambda i: i.endswith('txt'), txts))


dd = '/home/jtimm/gutenberg/data/text/'
out = '/home/jtimm/gutenberg/data/annotations/'
books = 'https://raw.githubusercontent.com/jaytimm/gbr/main/data/book_details.csv'
books1 = pd.read_csv(books)

prefix = 'PG'
suffix = '_text.txt'
books1['gutenberg_id'] = prefix + books1['gutenberg_id'].astype(str)
books1['fpath'] = books1['gutenberg_id'].astype(str) + suffix
books2 = books1[~books1['gutenberg_id'].isin(yas0)]
books2 = books2[books2['fpath'].isin(txts)]

lls = books2['fpath'].values.tolist()


for i in tqdm(range(len(lls))):
  
  fn = dd+lls[i]
  gid = re.sub("_.*$", "", lls[i])
  ## As function -- clean text
  with open(fn, 'r') as file:
      # Reading the file
      content = file.read()
      # Spliting the file
      splited_data = content.splitlines()
      # # Joining the lines
      clean_content = ''.join(content)
      
  pattern = re.compile(r'\n+')
  clean_content = re.sub(pattern, ' ', clean_content)
  clean_content0 = clean_content.strip()
  clean_content1 = pd.Series(clean_content0)
  
  # Splitting Text into Sentences
  def split_text_into_sentences(text):
      sentences = nltk.sent_tokenize(text)
      return sentences
  
  parts = split_text_into_sentences(clean_content0)
  
  SLICE_SIZE = 200 
  chunks = []
  reminder = len(parts) % SLICE_SIZE
  chunks_count = int((len(parts) - reminder) / SLICE_SIZE)
  for x in range(chunks_count):
      chunks.append(parts[x * SLICE_SIZE: (x+1) * SLICE_SIZE])
  if reminder:
      chunks.append(parts[-reminder:])
      
  output_list = [[(' '.join(inner_list))] for inner_list in chunks]
  pydf = pd.DataFrame(output_list, index = None, columns =['text'])
  ptexts = list(pydf['text'])
  
  ##########
  docs = []
  for piece in ptexts:
    doc = nlp(piece)
    docs.append(doc)
  
  merged = doc.from_docs(docs)
  
  
  from dframcy import DframCy
  dframcy = DframCy(nlp)
  
  spacy_df = dframcy.to_dataframe(merged, ['is_sent_start', 'id', 'text', 'lemma_', "pos_", "tag_",]).reset_index()
  spacy_df.token_is_sent_start = spacy_df.token_is_sent_start.astype(bool).cumsum() - 1
  
  spacy_df = spacy_df.rename(columns={'token_is_sent_start': 'sentence_id','index': 'token_id', 'token_text': 'token', 'token_lemma_': 'lemma', 'token_pos_': 'pos', 'token_tag_': 'tag'})
  
  spacy_df["gutenberg_id"] = gid
  spacy_df = spacy_df[['gutenberg_id', 'sentence_id', 'token_id', 'token', 'lemma', 'pos', 'tag']]
  an_name = out + gid + '_an.csv'
  pd.DataFrame.to_csv(spacy_df, an_name, index = False)
  
