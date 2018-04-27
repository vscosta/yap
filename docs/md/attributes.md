 Attributed Variables and Co-Routining   {#attributes}
=======================================
@ingroup extensions


YAP supports attributed variables, originally developed at OFAI by
Christian Holzbaur. Attributes are a means of declaring that an
  arbitrary term is a property for a variable. These properties can be
updated during forward execution. Moreover, the unification algorithm is
aware of attributed variables and will call user defined handlers when
  trying to unify these variables.

Attributed variables provide an elegant abstraction over which one can
extend Prolog systems. Their main application so far has been in
implementing constraint handlers, such as Holzbaur's CLPQR, Fruewirth
and Holzbaur's CHR, and CLP(BN).

Different Prolog systems implement attributed variables in different
ways. Originally, YAP  used the interface designed by SICStus
Prolog. This interface is still
available through the <tt>atts</tt> library, and is used by CLPBN.

From YAP-6.0.3 onwards we recommend using the hProlog, SWI style
interface. We believe that this design is easier to understand and
work with. Most packages included in YAP that use attributed
variables, such as CHR, CLP(FD), and CLP(QR), rely on the SWI-Prolog
interface.

+ @ref SICS_attributes
+ @ref New_Style_Attribute_Declarations
+ @ref CohYroutining
+ @ref AttributeVariables_Builtins



@}
