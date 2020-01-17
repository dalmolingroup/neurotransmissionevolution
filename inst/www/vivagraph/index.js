function easyLayout(graph_json) {
  console.log(graph_json);

  var nodeLoadTransform = (function() {
    var idKey = Object.keys(graph_json.nodes[0])[0];
    return function(node) {
      return {
        id: node[idKey],
        data: node
      };
    };
  })();

  var linkLoadTransform = (function() {
    var from = Object.keys(graph_json.links[0])[0];
    var to = Object.keys(graph_json.links[0])[1];
    return function(link) {
      return {
        fromId: link[from],
        toId: link[to],
        data: link
      };
    };
  })();

  var graph = Viva.Graph.serializer().loadFromJSON(
    graph_json,
    nodeLoadTransform,
    linkLoadTransform
  );

  var layout = Viva.Graph.Layout.forceDirected(graph, {
    springLength: 100, //30
    springCoeff: 0.0002, //0.0008
    gravity: -0.4, //-1.2
    theta: 0.4, //0.8
    dragCoeff: 0.05, //0.02
    timeStep: 5 //20
  });

  var pinned_node_count = 0;

  graph.forEachNode(function(node) {

    if ("x" in node.data){
      layout.setNodePosition(node.id, node.data.x, node.data.y);
      // console.log(`set node "${node.id}" position at x = ${node.data.x} and y = ${node.data.y}`);
    } else {
      layout.setNodePosition(node.id, Math.random() * 500, Math.random() * 500);
    }

    if(node.data.pinned){
      layout.pinNode(node, true);
      console.log(`${pinned_node_count++}. pinned node "${node.id}" position at x = ${node.data.x} and y = ${node.data.y}`);
    }

  });

  var graphics = Viva.Graph.View.webglGraphics();

  graphics.node(function(node) {

    var color = "#000000"

    if (node.data.color)
      color = node.data.color

    if (node.data.size)
      return Viva.Graph.View.webglSquare(node.data.size, color);

    else return Viva.Graph.View.webglSquare(5, color);

  });

  var renderer = Viva.Graph.View.renderer(graph, {
    container: document.getElementById("graph-container"),
    graphics: graphics,
    layout: layout,
    prerender: true
  });

  var events = Viva.Graph.webglInputEvents(graphics, graph);

  var selectedNodes = [];

  var initialPosition = { x: 0, y: 0 };

  events
    .mouseDown(function(draggedNode) {
      initialPosition = Object.assign(
        {},
        layout.getNodePosition(draggedNode.id)
      );
      console.log(
        `Mouse down on node: ${draggedNode.id} / ${JSON.stringify(
          initialPosition
        )}`
      );
    })
    .mouseMove(function(draggedNode) {
      //lala
    })
    .mouseUp(function(draggedNode) {
      graph.forEachNode(function(iteratedNode) {
        if (
          iteratedNode.selected &&
          draggedNode &&
          iteratedNode.id != draggedNode.id
        ) {
          console.log(
            `Mouse up on node: ${draggedNode.id} / ${JSON.stringify(
              layout.getNodePosition(draggedNode.id)
            )}`
          );
          console.log(`Initial position: ${JSON.stringify(initialPosition)}`);

          var iteratedNodePosition = layout.getNodePosition(iteratedNode.id);

          var draggedNodePosition = layout.getNodePosition(draggedNode.id);

          layout.setNodePosition(
            iteratedNode.id,
            iteratedNodePosition.x +
              (draggedNodePosition.x - initialPosition.x),
            iteratedNodePosition.y + (draggedNodePosition.y - initialPosition.y)
          );
        }
      });
    });

    renderer.run();
    renderer.pause();

  //////////////////////////////////
  // force layout controls
  //////////////////////////////////
  var controls = document.querySelectorAll(".control .layout");

  for (var i = 0; i < controls.length; i++) {
    controls[i].oninput = function() {
      layout.simulator[this.id](this.value);
      console.log(this.id, this.value);
    };
  }

  document.getElementById("remove-overlaps").onchange = function() {
    if (this.checked)
      Viva.Graph.Layout.removeOverlaps(layout, { active: true });
    else Viva.Graph.Layout.removeOverlaps(layout, { active: false });
  };

  document.getElementById("run").onchange = function() {
    if (this.checked) {
      // renderer.run();
      renderer.resume();
    }
    else renderer.pause();
  };

  document.getElementById("get").onclick = function() {
    // coords.value = "";
    var coordinates = [];
    graph.forEachNode(function(node) {
      var pos = layout.getNodePosition(node.id);
      // coords.value += node.id + "\t" + pos.x + "\t" + pos.y + "\n";
      coordinates.push({ x: pos.x, y: pos.y });
    });

    Shiny.setInputValue("coordinates", coordinates);
  };

  //////////////////////////////////
  // area selection
  //////////////////////////////////
  var multiSelectOverlay;

  document.addEventListener("keydown", function(e) {
    if (e.which === 16 && !multiSelectOverlay) {
      // shift key
      multiSelectOverlay = startMultiSelect(
        graph,
        renderer,
        layout,
        selectedNodes
      );
    }
  });
  document.addEventListener("keyup", function(e) {
    if (e.which === 16 && multiSelectOverlay) {
      multiSelectOverlay.destroy();
      multiSelectOverlay = null;
    }
  });
}

// var graph_json = {
//   nodes: [{ id: "a", x: 1, y: 1, size: 20 }, { id: "b" }],
//   links: [{ fromId: "a", toId: "b" }]
// }
