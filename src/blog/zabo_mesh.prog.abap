*&---------------------------------------------------------------------*
*& Report zabo_mesh
*&---------------------------------------------------------------------*
*& Author: Jerry Wang
*& Source: https://blogs.sap.com/2013/12/06/abap-mesh-in-740-connect-your-internal-table-as-bo-node-association/
*& Info: https://blogs.sap.com/2014/02/06/abap-news-for-release-740-sp05/
*&---------------------------------------------------------------------*
REPORT zabo_mesh.


TYPES: BEGIN OF t_manager,
         name   TYPE char10,
         salary TYPE int4,
       END OF t_manager,
       tt_manager TYPE SORTED TABLE OF t_manager WITH UNIQUE KEY name,
       BEGIN OF t_developer,
         name    TYPE char10,
         salary  TYPE int4,
         manager TYPE char10,
       END OF t_developer,
       tt_developer TYPE SORTED TABLE OF t_developer WITH UNIQUE KEY name,
       BEGIN OF MESH t_team,
         managers   TYPE tt_manager
             ASSOCIATION my_employee TO developers ON manager = name,
         developers TYPE tt_developer
           ASSOCIATION my_manager TO managers ON name = manager,
       END OF MESH t_team.

DATA: lt_developer TYPE tt_developer,
      lt_manager   TYPE tt_manager,
      ls_crm_team  TYPE t_team.

DATA(jerry) = VALUE t_developer( name = 'Jerry' salary = 1000 manager = 'Jason' ).
DATA(tom) = VALUE t_developer( name = 'Tom' salary = 2000 manager = 'Jason' ).
DATA(bob) = VALUE t_developer( name = 'Bob' salary = 2100 manager = 'Jason' ).
DATA(jack) = VALUE t_developer( name = 'Jack' salary = 1000 manager = 'Thomas' ).
DATA(david) = VALUE t_developer( name = 'David' salary = 2000 manager = 'Thomas' ).
DATA(john) = VALUE t_developer( name = 'John' salary = 2100 manager = 'Thomas' ).

DATA(jason) = VALUE t_manager( name = 'Jason' salary = 3000 ).
DATA(thomas) = VALUE t_manager( name = 'Thomas' salary = 3200 ).

INSERT jerry INTO TABLE lt_developer.
INSERT tom INTO TABLE lt_developer.
INSERT bob INTO TABLE lt_developer.
INSERT jack INTO TABLE lt_developer.
INSERT david INTO TABLE lt_developer.
INSERT john INTO TABLE lt_developer.

INSERT jason INTO TABLE lt_manager.
INSERT thomas INTO TABLE lt_manager.

ls_crm_team-developers = lt_developer.
ls_crm_team-managers = lt_manager.


" Use ABAP-Mesh
DATA(line) = ls_crm_team-developers\my_manager[ jerry ].
WRITE: / |Jerry manager name: { line-name }, Salary: { line-salary } (MESH)|.

" Use old way
READ TABLE ls_crm_team-developers ASSIGNING FIELD-SYMBOL(<employee>) WITH KEY name = 'Jerry'.
CHECK sy-subrc = 0.
READ TABLE ls_crm_team-managers ASSIGNING FIELD-SYMBOL(<manager>) WITH KEY name = <employee>-manager.
CHECK sy-subrc = 0.
* Question: why do I need a double space after Salary to line up with above example?
WRITE: / |Jerry manager name: { <manager>-name }, Salary:  { <manager>-salary } (OLD)|.

" Use ABAP-Mesh
LOOP AT ls_crm_team-managers\my_employee[ thomas ] ASSIGNING <employee>.
  WRITE: / |Thomas employee name: { <employee>-name } (MESH) |.
ENDLOOP.

" Use old way
LOOP AT ls_crm_team-developers ASSIGNING FIELD-SYMBOL(<developer>) WHERE manager = 'Thomas'.
  WRITE: / |Thomas employee name: { <developer>-name } (OLD) |.
ENDLOOP.
