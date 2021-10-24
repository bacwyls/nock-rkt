import sys

# is this string just a natural number?
def is_atom(s):
  for c in s:
    if c not in "0123456789.":
      return False
  return True

# is this is a cell that starts and ends with "[" and "]"?
# is this an xplicit cell
def is_xcell(s):
  l = len(s)
  bcnt = 0
  
  if s[0] != "[":
    return False

  for (i, c) in enumerate(s):
    if c == "[":
      bcnt += 1
    if c == "]":
      bcnt -= 1

    if bcnt == 0:
      return i == l - 1
    

# split on the fist space with bcnt == 0
# aka split after the first noun
def split_cell(s):
  if is_xcell(s):
    s = s[1:-1]

  bcnt = 0
  for (i, c) in enumerate(s):
    if c == "[":
      bcnt += 1
    if c == "]":
      bcnt -= 1

    if c == " " and bcnt == 0:
      return [s[:i], s[i+1:]]

  print("FAILED: " + s)
  return ["-1", "-1"]


# takes a string in nocks noun syntax
# returns a python list tree of the noun
def parse_noun(s):
  if is_atom(s):
    s = s.replace(".", "")
    return int(s)

  [h, t] = split_cell(s)
  return [parse_noun(h), parse_noun(t)]


# convert python-list-tree-noun
# to a string that nock.rkt will like
def to_rkt(n):
  if type(n) == int:
    return (str(n))

  return "(cell " + to_rkt(n[0]) + " " + to_rkt(n[1]) + ")"

# convert to explicitly defined cell
def to_xnoun(n):
  if type(n) == int:
    return str(n)

  return "[" + to_xnoun(n[0]) + " " + to_xnoun(n[1]) + "]"
  



if __name__ == '__main__':
  noun = sys.argv[1]
  noun = parse_noun(noun)
  noun = to_rkt(noun)
  print(noun)

