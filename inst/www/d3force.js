// function createV4SelectableForceDirectedGraph(svg, graph) {
// if both d3v3 and d3v4 are loaded, we'll assume
// that d3v4 is called d3v4, otherwise we'll assume
// that d3v4 is the default (d3)
if (typeof d3v4 == "undefined") d3v4 = d3;

let parentWidth = d3v4.select("svg").node().parentNode.clientWidth;
let parentHeight = d3v4.select("svg").node().parentNode.clientHeight;

var svg = d3v4
  .select("svg")
  .attr("width", parentWidth)
  .attr("height", parentHeight);

var width = +svg.attr("width"),
  height = +svg.attr("height");

var width = +svg.node().getBoundingClientRect().width;
var height = +svg.node().getBoundingClientRect().height;

// remove any previous graphs
svg.selectAll(".g-main").remove();

var gMain = svg.append("g").classed("g-main", true);

var rect = gMain
  .append("rect")
  .attr("width", parentWidth)
  .attr("height", parentHeight)
  .style("fill", "white");

var gDraw = gMain.append("g");

var zoom = d3v4.zoom().on("zoom", zoomed);

gMain.call(zoom);

function zoomed() {
  gDraw.attr("transform", d3v4.event.transform);
}

var color = d3v4.scaleOrdinal(d3v4.schemeCategory20);

//   if (!("links" in graph)) {
//     console.log("Graph is missing links");
//     return;
//   }

//   var nodes = {};
//   var i;
//   for (i = 0; i < graph.nodes.length; i++) {
//     nodes[graph.nodes[i].id] = graph.nodes[i];
//     graph.nodes[i].weight = 1.01;
//   }
// svg objects
var link, node;
// the data - an object with nodes and links
var graph;

// the brush needs to go before the nodes so that it doesn't
// get called when the mouse is over a node
var gBrushHolder = gDraw.append("g");
var gBrush = null;

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

var stopped = false;

var simulation = d3v4.forceSimulation();

Shiny.addCustomMessageHandler("dataTransferredFromServer", function(_graph){//});
// d3.json("www/miserables.json", function(error, _graph) {
//   if (error) throw error;
  graph = _graph;

  // initializeDisplay();
  // initializeSimulation();
  link = gDraw
    .append("g")
    .attr("class", "link")
    .selectAll("line")
    .data(graph.links)
    .enter()
    .append("line")
    .attr("stroke-width", function(d) {
      return Math.sqrt(d.value);
    });

  node = gDraw
    .append("g")
    .attr("class", "node")
    .selectAll("circle")
    .data(graph.nodes)
    .enter()
    .append("circle")
    .attr("r", 5)
    .attr("fill", function(d) {
      if ("color" in d) return d.color;
      else return color(d.group);
    })
    .call(
      d3v4
        .drag()
        .on("start", dragstarted)
        .on("drag", dragged)
        .on("end", dragended)
    );

  // add titles for mouseover blurbs
  node.append("title").text(function(d) { return d.id });

//   simulation
//     .force("link", d3v4.forceLink())
//     .force("charge", d3v4.forceManyBody())
//     // .force("center", d3v4.forceCenter(parentWidth / 2, parentHeight / 2))
//     // .force("x", d3v4.forceX(parentWidth / 2))
//     // .force("y", d3v4.forceY(parentHeight / 2));
//     .force("center", d3v4.forceCenter())
//     .force("x", d3v4.forceX())
//     .force("y", d3v4.forceY());
  
  simulation
  .force("link", d3v4.forceLink())
  .force("charge", d3v4.forceManyBody())
  .force("collide", d3v4.forceCollide())
  .force("center", d3v4.forceCenter())
  .force("forceX", d3v4.forceX())
  .force("forceY", d3v4.forceY());

  simulation.nodes(graph.nodes).on("tick", ticked);

  simulation.force("link").links(graph.links);
});

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
    .strength(forceProperties.collide.strength * forceProperties.collide.enabled)
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

// update the display based on the forces (but not positions)
function updateDisplay() {
  node
    // .attr("r", function(d) { return (d.size || 1) * forceProperties.collide.radius })
    .attr("r", function(d) {
      return d.size || 5;
    })
    // .attr("r", forceProperties.collide.radius)
    .attr("stroke", forceProperties.charge.strength > 0 ? "blue" : "red");
  // .attr("stroke-width", forceProperties.charge.enabled==false ? 0 : Math.abs(forceProperties.charge.strength)/15);

  link.attr("stroke-width", forceProperties.link.enabled ? 1 : 0.5).attr("opacity", forceProperties.link.enabled ? 1 : 0);
}

// convenience function to update everything (run after UI input)
function updateAll() {
  updateForces();
  updateDisplay();
}

function ticked() {
  // update node and line positions at every step of
  // the force simulation
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

var brushMode = false;
var brushing = false;

var brush = d3v4
  .brush()
  .on("start", brushstarted)
  .on("brush", brushed)
  .on("end", brushended);

function brushstarted() {
  // keep track of whether we're actively brushing so that we
  // don't remove the brush on keyup in the middle of a selection
  brushing = true;

  node.each(function(d) {
    d.previouslySelected = shiftKey && d.selected;
  });
}

rect.on("click", () => {
  node.each(function(d) {
    d.selected = false;
    d.previouslySelected = false;
  });
  node.classed("selected", false);
});

function brushed() {
  if (!d3v4.event.sourceEvent) return;
  if (!d3v4.event.selection) return;

  var extent = d3v4.event.selection;

  node.classed("selected", function(d) {
    return (d.selected = d.previouslySelected ^ (extent[0][0] <= d.x && d.x < extent[1][0] && extent[0][1] <= d.y && d.y < extent[1][1]));
  });
}

function brushended() {
  if (!d3v4.event.sourceEvent) return;
  if (!d3v4.event.selection) return;
  if (!gBrush) return;

  gBrush.call(brush.move, null);

  if (!brushMode) {
    // the shift key has been release before we ended our brushing
    gBrush.remove();
    gBrush = null;
  }

  brushing = false;
}

d3v4.select("body").on("keydown", keydown);
d3v4.select("body").on("keyup", keyup);

var shiftKey;

function keydown() {
  shiftKey = d3v4.event.shiftKey;

  if (shiftKey) {
    // if we already have a brush, don't do anything
    if (gBrush) return;

    brushMode = true;

    if (!gBrush) {
      gBrush = gBrushHolder.append("g");
      gBrush.call(brush);
    }
  }
}

function keyup() {
  shiftKey = false;
  brushMode = false;

  if (!gBrush) return;

  if (!brushing) {
    // only remove the brush if we're not actively brushing
    // otherwise it'll be removed when the brushing ends
    gBrush.remove();
    gBrush = null;
  }
}

function dragstarted(d) {
  if (!d3v4.event.active) simulation.alphaTarget(0.9).restart();

  if (!d.selected && !shiftKey) {
    // if this node isn't selected, then we have to unselect every other node
    node.classed("selected", function(p) {
      return (p.selected = p.previouslySelected = false);
    });
  }

  d3v4.select(this).classed("selected", function(p) {
    d.previouslySelected = d.selected;
    return (d.selected = true);
  });

  node
    .filter(function(d) {
      return d.selected;
    })
    .each(function(d) {
      //d.fixed |= 2;
      d.fx = d.x;
      d.fy = d.y;
    });
}

function dragged(d) {
  //d.fx = d3v4.event.x;
  //d.fy = d3v4.event.y;
  node
    .filter(function(d) {
      return d.selected;
    })
    .each(function(d) {
      d.fx += d3v4.event.dx;
      d.fy += d3v4.event.dy;
    });
}

function dragended(d) {
  if (!d3v4.event.active) simulation.alphaTarget(0);
  //   d.fx = null;
  //   d.fy = null;
  //   node
  //     .filter(function(d) {
  //       return d.selected;
  //     })
  //     .each(function(d) {
  //       //d.fixed &= ~6;
  //       d.fx = null;
  //       d.fy = null;
  //     });
}

// var texts = ["Use the scroll wheel to zoom", "Hold the shift key to select nodes"];

// svg
//   .selectAll("text")
//   .data(texts)
//   .enter()
//   .append("text")
//   .attr("x", 900)
//   .attr("y", function(d, i) {
//     return 470 + i * 18;
//   })
//   .text(function(d) {
//     return d;
//   });

d3.select("#done").on("click", function() {
  var coordinates = [];
  node.each(function(d) {
    coordinates.push({ id: d.id, x: d.x, y: d.y });
  });

  Shiny.setInputValue("coordinates", coordinates);
});

d3.select("#pause").on("click", function() {
  if (stopped) {
    simulation.restart();

    node.each(function(d) {
      d.fx = null;
      d.fy = null;
    });

    console.log("restarted");
  } else {
    simulation.stop();

    node.each(function(d) {
      d.fx = d.x;
      d.fy = d.y;
    });

    console.log("stopped");
  }
  stopped = !stopped;
});

//   return graph;
// }
