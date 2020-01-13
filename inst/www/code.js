var width = 960,
    height = 500,
    shiftKey;

var svg = d3.select("body")
.attr("tabindex", 1)
.on("keydown.brush", keydowned)
.on("keyup.brush", keyupped)
.each(function() { this.focus(); })
.append("svg")
  .attr("width", width)
  .attr("height", height)
  .call(
    d3.zoom().on("zoom", function() {
      svg.attr("transform", d3.event.transform);
    })
  )
  .append("g");
  
var brush = svg.append("g")
.attr("class", "brush");

// var width = +svg.node().getBoundingClientRect().width;
// var height = +svg.node().getBoundingClientRect().height;

// svg objects
var link, node;
// the data - an object with nodes and links
var graph;

// load the data
d3.json("www/miserables.json", function(error, _graph) {
  if (error) throw error;
  graph = _graph;
  initializeDisplay();
  initializeSimulation();
});

// function easyLayout(graph_json) {
//   console.log(graph_json);
//   // load the data
//   graph = graph_json;
//   initializeDisplay();
//   initializeSimulation();
// }

//////////// FORCE SIMULATION ////////////

// force simulator
var simulation = d3.forceSimulation();

// set up the simulation and event to update locations after each tick
function initializeSimulation() {
  simulation.nodes(graph.nodes);
  initializeForces();
  simulation.on("tick", ticked);
}

// values for all forces
forceProperties = {
  center: {
    x: 0.5,
    y: 0.5
  },
  charge: {
    enabled: true,
    strength: -30,
    distanceMin: 1,
    distanceMax: 2000
  },
  collide: {
    enabled: true,
    strength: 0.7,
    iterations: 1,
    radius: 5
  },
  forceX: {
    enabled: false,
    strength: 0.1,
    x: 0.5
  },
  forceY: {
    enabled: false,
    strength: 0.1,
    y: 0.5
  },
  link: {
    enabled: true,
    distance: 30,
    iterations: 1
  }
};

// add forces to the simulation
function initializeForces() {
  // add forces and associate each with a name
  simulation
    .force("link", d3.forceLink())
    .force("charge", d3.forceManyBody())
    .force("collide", d3.forceCollide())
    .force("center", d3.forceCenter())
    .force("forceX", d3.forceX())
    .force("forceY", d3.forceY());
  // apply properties to each of the forces
  updateForces();
}

// apply new force properties
function updateForces() {
  // get each force by name and update the properties
  simulation
    .force("center")
    .x(width * forceProperties.center.x)
    .y(height * forceProperties.center.y);
  simulation
    .force("charge")
    .strength(forceProperties.charge.strength * forceProperties.charge.enabled)
    .distanceMin(forceProperties.charge.distanceMin)
    .distanceMax(forceProperties.charge.distanceMax);
  simulation
    .force("collide")
    .strength(
      forceProperties.collide.strength * forceProperties.collide.enabled
    )
    // .radius(forceProperties.collide.radius)
    .radius(function(d) {
      return (d.size || 1) * forceProperties.collide.radius;
    })
    .iterations(forceProperties.collide.iterations);
  simulation
    .force("forceX")
    .strength(forceProperties.forceX.strength * forceProperties.forceX.enabled)
    .x(width * forceProperties.forceX.x);
  simulation
    .force("forceY")
    .strength(forceProperties.forceY.strength * forceProperties.forceY.enabled)
    .y(height * forceProperties.forceY.y);
  simulation
    .force("link")
    .id(function(d) {
      return d.id;
    })
    .distance(forceProperties.link.distance)
    .iterations(forceProperties.link.iterations)
    .links(forceProperties.link.enabled ? graph.links : []);

  // updates ignored until this is run
  // restarts the simulation (important if simulation has already slowed down)
  simulation.alpha(1).restart();
}

//////////// DISPLAY ////////////

// generate the svg objects and force simulation
function initializeDisplay() {
    brush.call(d3.brush()
    .extent([[0, 0], [width, height]])
    .on("start", brushstarted)
    .on("brush", brushed)
    .on("end", brushended));

  // set the data and properties of link lines
  link = svg
    .append("g")
    .attr("class", "links")
    .selectAll("line")
    .data(graph.links)
    .enter()
    .append("line");

  // set the data and properties of node circles
  node = svg
    .append("g")
    .attr("class", "nodes")
    .selectAll("circle")
    .data(graph.nodes)
    .enter()
    .append("circle")
    .call(
      d3
        .drag()
        // .on("start", dragstarted)
        .on("drag", dragged)
        // .on("end", dragended)
    );

  // node tooltip
  node.append("title").text(function(d) {
    return d.id;
  });
  // visualize the graph
  updateDisplay();
}

// update the display based on the forces (but not positions)
function updateDisplay() {
  node
    .attr("r", function(d) {
      return (d.size || 1) * forceProperties.collide.radius;
    })
    // .attr("r", forceProperties.collide.radius)
    .attr("stroke", forceProperties.charge.strength > 0 ? "blue" : "red")
    .attr(
      "stroke-width",
      forceProperties.charge.enabled == false
        ? 0
        : Math.abs(forceProperties.charge.strength) / 15
    );

  link
    .attr("stroke-width", forceProperties.link.enabled ? 1 : 0.5)
    .attr("opacity", forceProperties.link.enabled ? 1 : 0);
}

// update the display positions after each simulation tick
function ticked() {
  link
    .attr("x1", function(d) {
      return d.source.x;
    })
    .attr("y1", function(d) {
      return d.source.y;
    })
    .attr("x2", function(d) {
      return d.target.x;
    })
    .attr("y2", function(d) {
      return d.target.y;
    });

  node
    .attr("cx", function(d) {
      return d.x;
    })
    .attr("cy", function(d) {
      return d.y;
    });
  d3.select("#alpha_value").style("flex-basis", simulation.alpha() * 100 + "%");
}

//////////// UI EVENTS ////////////

// function dragstarted(d) {
//   if (!d3.event.active) simulation.alphaTarget(0.3).restart();
//   d.fx = d.x;
//   d.fy = d.y;
// }

// function dragged(d) {
//   d.fx = d3.event.x;
//   d.fy = d3.event.y;
// }

// function dragended(d) {
//   if (!d3.event.active) simulation.alphaTarget(0.0001);
//   d.fx = null;
//   d.fy = null;
// }

// update size-related forces
d3.select(window).on("resize", function() {
  width = +svg.node().getBoundingClientRect().width;
  height = +svg.node().getBoundingClientRect().height;
  updateForces();
});

// convenience function to update everything (run after UI input)
function updateAll() {
  updateForces();
  updateDisplay();
}


/////////////////////////////////


function brushstarted() {
    if (d3.event.sourceEvent.type !== "end") {
      node.classed("selected", function(d) {
        return d.selected = d.previouslySelected = shiftKey && d.selected;
      });
    }
  }

  function brushed() {
    if (d3.event.sourceEvent.type !== "end") {
      var selection = d3.event.selection;
      node.classed("selected", function(d) {
        return d.selected = d.previouslySelected ^
            (selection != null
            && selection[0][0] <= d.x && d.x < selection[1][0]
            && selection[0][1] <= d.y && d.y < selection[1][1]);
      });
    }
  }

  function brushended() {
    if (d3.event.selection != null) {
      d3.select(this).call(d3.event.target.move, null);
    }
  }

  function mousedowned(d) {
    if (shiftKey) {
      d3.select(this).classed("selected", d.selected = !d.selected);
      d3.event.stopImmediatePropagation();
    } else if (!d.selected) {
      node.classed("selected", function(p) { return p.selected = d === p; });
    }
  }

  function dragged(d) {
    nudge(d3.event.dx, d3.event.dy);
  }

  function nudge(dx, dy) {
    node.filter(function(d) { return d.selected; })
        .attr("cx", function(d) { return d.x += dx; })
        .attr("cy", function(d) { return d.y += dy; })
  
    link.filter(function(d) { return d.source.selected; })
        .attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; });
  
    link.filter(function(d) { return d.target.selected; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });
  }
  
  function keydowned() {
    if (!d3.event.metaKey) {
      switch (d3.event.keyCode) {
        case 38: nudge( 0, -1); break; // UP
        case 40: nudge( 0, +1); break; // DOWN
        case 37: nudge(-1,  0); break; // LEFT
        case 39: nudge(+1,  0); break; // RIGHT
      }
    }
    shiftKey = d3.event.shiftKey || d3.event.metaKey;
  }
  
  function keyupped() {
    shiftKey = d3.event.shiftKey || d3.event.metaKey;
  }