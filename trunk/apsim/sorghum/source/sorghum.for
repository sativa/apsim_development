      module sorghummodule
      use croplibrary
      use cropmoddata

      contains

      include 'cropmodmain.for'
      include 'sorgmain.for'
      include 'sorgn.for'
      include 'sorgopt.for'
      include 'sorgtree.for'

      end module