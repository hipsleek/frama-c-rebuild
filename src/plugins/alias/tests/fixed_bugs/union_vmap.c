// from open-source-case-studies/chrony

char* CPS_SplitWord(char *line)
{
  return line;
}

void CPS_ParseKey()
{
	char **key;
  char *s2 = CPS_SplitWord("a");
  char *s3 = CPS_SplitWord("b");
  if (*s3) {
    *key = s3;
  }
  else {
    *key = s2;
  }
}


