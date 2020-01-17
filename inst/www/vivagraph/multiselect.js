function startMultiSelect(graph, renderer, layout, selectedNodes) {
    var graphics = renderer.getGraphics();
    var domOverlay = document.querySelector('.graph-overlay');
    var overlay = createOverlay(domOverlay);
    overlay.onAreaSelected(handleAreaSelected);

    return overlay;

    function handleAreaSelected(area) {
        // For the sake of this demo we are using silly O(n) implementation.
        // Could be improved with spatial indexing if required.
        var topLeft = graphics.transformClientToGraphCoordinates({
            x: area.x,
            y: area.y
        });

        var bottomRight = graphics.transformClientToGraphCoordinates({
            x: area.x + area.width,
            y: area.y + area.height
        });

        selectedNodes = [];

        graph.forEachNode(higlightIfInside);

        console.log(selectedNodes);

        renderer.rerender();

        return;

        function higlightIfInside(node) {
            var nodeUI = graphics.getNodeUI(node.id);
            if (isInside(node.id, topLeft, bottomRight)) {

                node.selected = true;

                nodeUI.color = 0xFFA500ff;

                selectedNodes.push(node);

                console.log(node);

                // nodeUI.size = 20;
            } else {
                node.selected = false;
                nodeUI.color = 0x000000ff;
                // nodeUI.size = 10;
            }
        }

        function isInside(nodeId, topLeft, bottomRight) {
            var nodePos = layout.getNodePosition(nodeId);
            return (topLeft.x < nodePos.x && nodePos.x < bottomRight.x &&
                topLeft.y < nodePos.y && nodePos.y < bottomRight.y);
        }
    }
}

function createOverlay(overlayDom) {
    var selectionClasName = 'graph-selection-indicator';
    var selectionIndicator = overlayDom.querySelector('.' + selectionClasName);
    if (!selectionIndicator) {
        selectionIndicator = document.createElement('div');
        selectionIndicator.className = selectionClasName;
        overlayDom.appendChild(selectionIndicator);
    }

    var notify = [];
    var dragndrop = Viva.Graph.Utils.dragndrop(overlayDom);
    var selectedArea = {
        x: 0,
        y: 0,
        width: 0,
        height: 0
    };
    var startX = 0;
    var startY = 0;

    dragndrop.onStart(function (e) {
        startX = selectedArea.x = e.clientX;
        startY = selectedArea.y = e.clientY;
        selectedArea.width = selectedArea.height = 0;

        updateSelectedAreaIndicator();
        selectionIndicator.style.display = 'block';
    });

    dragndrop.onDrag(function (e) {
        recalculateSelectedArea(e);
        updateSelectedAreaIndicator();
        notifyAreaSelected();
    });

    dragndrop.onStop(function () {
        selectionIndicator.style.display = 'none';
    });

    overlayDom.style.display = 'block';

    return {
        onAreaSelected: function (cb) {
            notify.push(cb);
        },
        destroy: function () {
            overlayDom.style.display = 'none';
            dragndrop.release();
        }
    };

    function notifyAreaSelected() {
        notify.forEach(function (cb) {
            cb(selectedArea);
        });
    }

    function recalculateSelectedArea(e) {
        selectedArea.width = Math.abs(e.clientX - startX);
        selectedArea.height = Math.abs(e.clientY - startY);
        selectedArea.x = Math.min(e.clientX, startX);
        selectedArea.y = Math.min(e.clientY, startY);
    }

    function updateSelectedAreaIndicator() {
        selectionIndicator.style.left = selectedArea.x + 'px';
        selectionIndicator.style.top = selectedArea.y + 'px';
        selectionIndicator.style.width = selectedArea.width + 'px';
        selectionIndicator.style.height = selectedArea.height + 'px';
    }
}
