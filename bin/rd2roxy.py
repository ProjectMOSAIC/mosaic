#!/opt/local/bin/python2.6
import re as re


from optparse import OptionParser

parser = OptionParser()

parser.add_option("-f", "--filename", dest="filename", default="",
                  help="input file", 
				  metavar="<file name>")

parser.add_option("-o", "--output", dest="outfilename", default="out.RR",
                  help="output file",
				  metavar="<file name>")


(options, args) = parser.parse_args()

origContents = open(options.filename, 'r').read()

#print type(origContents)
#print len(origContents)

modifiedContents = origContents
modifiedContents = re.sub(r'\\link{\s*([^\}]*)}', r'\link<<<\1>>>', modifiedContents)
modifiedContents = re.sub(r'\\code{\s*([^\}]*)}', r'\code<<<\1>>>', modifiedContents)
modifiedContents = re.sub(r'\\email{\s*([^\}]*)}', r'\email<<<\1>>>}', modifiedContents)
modifiedContents = re.sub(r'\s*\\(alias|name){\s*([^\}]*)}', r'', modifiedContents)
modifiedContents = re.sub(r'\s*\\title{([^\}]*)}', r'\1\n\n', modifiedContents)
modifiedContents = re.sub(r'\s*\\item{\s*([^\}]*)}.*{([^}]*)}', r'@param \1 \2\n', modifiedContents)
modifiedContents = re.sub(r'\s*\\description{\s*([^\}]*)}', r'\n\n\1\n', modifiedContents)
modifiedContents = re.sub(r'\s*\\usage{\s*([^\}]*)}', r'\n\n', modifiedContents)
modifiedContents = re.sub(r'\s*\\value{\s*([^\}]*)}', r'\n@return \1\n', modifiedContents)
modifiedContents = re.sub(r'\s*\\keyword{\s*([^\}]*)}', r'\n@keywords \1\n', modifiedContents)
modifiedContents = re.sub(r'\s*\\author{\s*([^\}]*)}', r'\n@author \1\n', modifiedContents)
modifiedContents = re.sub(r'\s*\\examples{\s*([^\}]*)}', r'\n@export\n@examples\n\1', modifiedContents)
modifiedContents = re.sub(r'\s*\\seealso{\s*([^\}]*)}', r'\n@seealso \1\n', modifiedContents)
modifiedContents = re.sub(r'@calvin', '@@calvin', modifiedContents)
modifiedContents = re.sub(r'@macalester', '@@macalester', modifiedContents)
modifiedContents = re.sub(r'@smith', '@@smith', modifiedContents)
modifiedContents = re.sub(r'\s*\\arguments{\s*([^\}]*)}', r'\n\1\n', modifiedContents)
modifiedContents = re.sub(r'<<<',r'{', modifiedContents)
modifiedContents = re.sub(r'>>>',r'}', modifiedContents)
modifiedContents = re.sub(r'\n', r"\n#' ", modifiedContents)

# print "="*60
print "#' ",
print  modifiedContents



