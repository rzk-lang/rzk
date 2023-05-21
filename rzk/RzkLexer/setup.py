from setuptools import setup, find_packages

setup (
  name='rzklexer',
  packages=find_packages(),
  entry_points =
  """
  [pygments.lexers]
  rzklexer = rzklexer:RzkLexer
  """,
)
