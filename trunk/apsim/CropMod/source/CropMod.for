      module cropmodmodule
      use cropmoddata
      use croplibrary

      contains

      include 'cropmodmain.for'
      include 'cropmodtree.for'
      include 'maize.for'
      include 'process.for'
      include 'sorg.for'
      include 'sunf.for'
      include 'wheat.for'

      end module cropmodmodule