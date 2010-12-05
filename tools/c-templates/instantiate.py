def _get_template (name):
   """
   Finds and reads the template with the given name.

   Presently, this finds a template simply by appending '.template' to
   the given name.
   """
   with open(name + '.template') as infile:
      return infile.read()


def _interpolate (template, params):
   """
   Lightweight string interpolation.  Replaces occurrences of
   substrings like ${FIELD} with their corresponding replacements.
   """
   import re
   return re.sub(r'\$\{([^}]*)\}',
                 lambda match: params[match.group(1)],
                 template)


def make_vector (params):
   """
   Creates a vector with the given parameters map, which should
   map strings to strings.  The following parameters must be defined:

       include_guard_name       the name for the guard in the header
       local_includes           local C include statements needed in the header
       namespace_prefix         the prefix to give all exported functions

       vector_type              the name for the generated vector type
       element_type             the name of the vector's element type

       header_name              the name for the generated header
       code_name                the name for the generated code file

   A header file and a code file are written to the names specified.
   """
   print "writing %s" % params['header_name']
   with open(params['header_name'], 'w') as header:
      header.write(_interpolate(_get_template("vector/vector.h"), params))

   print "writing %s" % params['code_name']
   with open(params['code_name'], 'w') as code:
      code.write(_interpolate(_get_template("vector/vector.c"), params))


def make_hash_table (params):
   """
   Creates a hash table with the given parameters map, which should
   map strings to strings.  The following parameters must be defined:

       include_guard_name       the name for the guard in the header
       local_includes           local C include statements needed in the header
       prefix                   the prefix to give all exported functions

       hash_table_type          the name for the generated hash table type
       key_type                 the name of the table's key type
       key_equal_func           the name of the equals test function on keys
       key_hash_func            the name of the hash function on keys
       value_type               the name of the table's value type

       header_name              the name for the generated header
       code_name                the name for the generated code file

   A header file and a code file are written to the names specified.
   """
   print "writing %s" % params['header_name']
   with open(params['header_name'], 'w') as header:
      header.write(_interpolate(_get_template("hash-table/hash_table.h"), params))

   print "writing %s" % params['code_name']
   with open(params['code_name'], 'w') as code:
      code.write(_interpolate(_get_template("hash-table/hash_table.c"), params))
