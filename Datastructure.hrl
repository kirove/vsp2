%% Records and Macros used by nodes

-record(node, {
name,
state = sleeping,
find_count = undefined,
fragment_id = undefined,
level = undefined
}).

-record(edge_management, {
edge_list,
in_branch_edge = undefined,
best_edge = undefined,
best_weight = undefined,
test_edge = undefined
}).

-record(edge, {
weight_id,
node_pid,
state = basic
}).