#!BPY
"""
Name: 'To Lisp'
Blender: 249
Group: 'Export'
Tooltip: 'Export to a READable Common Lisp assoc list'
"""

import Blender
import bpy

def export_to_lisp(filepath):
    out = file(filepath, 'w')
    sce = bpy.data.scenes.active
    ob = sce.objects.active
    mesh = ob.getData(mesh=1)
    out.write("((:vertices . (")
    for vert in mesh.verts:
        out.write( '\n#(%f %f %f)' % (vert.co.x,
                                          vert.co.y, vert.co.z))
    out.write("))\n")
    out.write('(:faces . (')
    for face in mesh.faces:
        out.write('\n(')
        for vert in face.v:
            out.write( '%i ' % (vert.index))
        out.write(')')
    out.write(")))")
    out.close()

Blender.Window.FileSelector(export_to_lisp, "Export")
