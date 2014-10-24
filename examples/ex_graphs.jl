# This implements the interface of the Graphs.jl package:
# http://graphsjl-docs.readthedocs.org/en/latest/interface.html

using Traits
using Graphs

@traitdef Graphs_Basic{G} begin
    vertex_type(G)
    edge_type(G)
    is_directed(G)
end

@traitdef Vertex_List{G} <: Graphs_Basic{G} begin
    num_vertices(G)
    vertices(G)
end

@traitdef Edge_List{G,E} <: Graphs_Basic{G} begin
    # returns the number of edges contained in g.
    num_edges(G) -> Int

    # returns an iterable view/container of all edges.
    edges(G)

    # returns the source vertex of an edge e in graph g.
    source(E, G)

    # returns the target vertex of an edge e in graph g.
    target(E, G)
end

@traitdef Edge_Map{G,E} <: Graphs_Basic{G} begin
    # returns the index of a vertex e in graph g.
    edge_index(E, G) -> Int
end

# # and more:
# vertex_map
# edge_map
# adjacency_list
# incidence_list
# bidirectional_adjacency_list
# bidirectional_incidence_list

# Definitions from Graphs.jl/test/edgelist.jl
pairs = [(1,2), (1,3), (2,3), (2,4), (3,5), (4,5), (2,5)]
eds = Edge{Int}[Edge(i,p[1],p[2]) for (i,p) in enumerate(pairs)]

gd = simple_edgelist(5, eds)
gu = simple_edgelist(5, eds; is_directed=false)

# check traits:
GD = typeof(gd)
GU = typeof(gu)
E = Edge{Int}

@assert istrait(Graphs_Basic{GD})
@assert istrait(Graphs_Basic{GU})
@assert istrait(Vertex_List{GD})
@assert istrait(Vertex_List{GU})
@assert istrait(Edge_List{GD, E})
@assert istrait(Edge_List{GU, E})

# use for dispatch:
@traitfn function f{G, E; Edge_List{G, E}, Vertex_List{G}}(g::G, e::E) 
    num_edges(g) + num_vertices(g) + edge_index(e)
end

try
    f(2, 3, 5) # doesn't work as Int are not graphs
catch e
    println(e)
end

f(gd, eds[5]) # -> 17
# note that f will work with any types which implement Edge_List{G, E}, Vertex_List{G}
