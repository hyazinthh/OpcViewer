var baseSize = point(1, 1);
var viewport = point(1, 1);
var drag = { elem: null, prev: null };
var resize = { elem: null, prev: null };

function point(x, y) {
    return { X: x, Y: y };
}

// Sets the viewport size
function setViewport(size) {
    viewport = size;
    transformAll();
}

// Sets the base size
function setBaseSize(size) {
    baseSize = size;
    transformAll();
}

// Transforms all labels and arrows
function transformAll() {
    $('.annotations .annotation').find('.label').each(function () {
        transform($(this));
    });

    $('.annotations .annotation').find('.arrow').each(function () {
        setArrowOrigin($(this));
    });
}

// Computes the scale of an element based on the current size
// of the viewport and the base size
function getScale() {
    return point(viewport.X / baseSize.X, viewport.Y / baseSize.Y);
}

// Gets the corresponding arrow of a label
function getArrow(label) {
    return label.parents('.annotation')
                .find('.arrow');
}

// Gets the corresponding label of an arrow
function getLabel(arrow) {
    return arrow.parents('.annotation')
                .find('.label');    
}

// Transforms the given label based on its scale and position
function transform(elem) {
    var pos = elem.data('pos');
    var scale = getScale();
    var globalScale = Math.min(scale.X, scale.Y);

    elem.css({
        'transform': `translate(${pos.X * scale.X}px, ${pos.Y * scale.Y}px) scale(${globalScale})`
    });    
}

// Adjusts the height of the given label based on its
// textarea
function autosize(elem) {
    elem.children('textarea')
		.height('auto')
        .height(function() {
            return `${this.scrollHeight}px`;
        });

    var arrow = getArrow(elem);
    if (arrow.length > 0) {
        setArrowOrigin(arrow);
    }     
}

// Sets the text of the given label
function setText(elem, text) {
    elem.children('textarea')
        .empty()
        .append(text);
        
    autosize(elem);
}

// Sets the width of the given label
function setWidth(elem, width) {
    elem.width(width);
    autosize(elem);
}

// Sets the position of the given label
function setPosition(elem, pos) {
    elem.data('pos', pos);
    transform(elem);

    var arrow = getArrow(elem);
    if (arrow.length > 0) {
        setArrowOrigin(arrow);
    }    
}

// Makes the given label the top label
function setTop(elem) {
    $('.annotations .top.label').each(function () {
        $(this).removeClass('top');
    });
    elem.addClass('top');
}

// Arrows
function setArrowOrigin(arrow) {
    var label = getLabel(arrow);
    if (label.length === 0) {
        return;
    }

    var scale = getScale();
    var globalScale = Math.min(scale.X, scale.Y);

    var p = label.data('pos');
    var w = label.width() * globalScale / scale.X;
    var h = label.height() * globalScale / scale.Y;

    var target = arrow.data('target');
    var center = point(p.X + w / 2, p.Y + h / 2);

    var anchors = [];
    anchors.push( point(center.X - w / 2, center.Y) );
    anchors.push( point(center.X + w / 2, center.Y) );
    anchors.push( point(center.X, center.Y - h / 2) );
    anchors.push( point(center.X, center.Y + h / 2) );

    var origin = anchors.map(function (a) {
        var dc = point(a.X - center.X, a.Y - center.Y);
        var dt = point(a.X - target.X, a.Y - target.Y);

        if (dc.X != 0) {
            dc = dc.X; dt = dt.X;
        } else {
            dc = dc.Y; dt = dt.Y;
        }

        if (Math.sign(dc) != Math.sign(dt)) {
            return { a:a, d:Math.abs(dt) };
        } else {
            return { a:a, d:-1 };
        }
    }).reduce(function (max, x) {
        return (x.d > max.d) ? x : max;
    }).a

    var midpoint = point(origin.X + 10 * Math.sign(origin.X - center.X), 
                         origin.Y + 10 * Math.sign(origin.Y - center.Y));

    arrow.data('origin', origin)
         .data('midpoint', midpoint);
    transformArrow(arrow);
}

function setArrowTarget(arrow, t) {
    arrow.data('target', t);
    setArrowOrigin(arrow);

    transformArrow(arrow);
}

function setArrowHeadSize(arrow, s) {
    var marker = arrow.children('defs')
                      .children('.arrowHead');

    marker.attr('viewBox', `0 0 ${s} ${s}`)
          .attr('markerWidth', s)
          .attr('markerHeight', s)
          .attr('refX', s * 0.75)
          .attr('refY', s / 2)
          .children('path')
          .attr('d', `M 0 0 L ${s} ${s / 2} L 0 ${s} z`);
    
    arrow.children('.arrowLine')
         .attr('marker-end', `url(#${marker.attr('id')})`);
}

function transformArrow(arrow) {
    var scale = getScale();
    var lineWidth = 3 * Math.min(scale.X, scale.Y, 1.0); 
    
    var pts = []
    pts.push(arrow.data('origin'));
    pts.push(arrow.data('midpoint'));
    pts.push(arrow.data('target'));

    pts.forEach(function (a, i) {
        pts[i] = point(a.X * scale.X, a.Y * scale.Y);
    });

    arrow.children('.arrowLine')
         .css('stroke-width', `${lineWidth}`)
         .attr('d', `M ${pts[0].X} ${pts[0].Y} ` +
                    `L ${pts[1].X} ${pts[1].Y} ` +
                    `L ${pts[2].X} ${pts[2].Y} `);
}

// Initializes the label for the Javascript
// side (dragging, resizing, ...)
function initLabel (elem) {
    if (elem.hasClass('disabled')) {
        return;
    }

    elem.on('mouseenter', function() { if (drag.elem === null) elem.addClass('hovered'); })
        .on('mouseleave', function() { elem.removeClass('hovered'); });

    elem.children('textarea')
		.on('focus', function() { elem.addClass('focused'); })
		.on('blur', function() { elem.removeClass('focused'); })
		.on('input', function() { autosize(elem); });

    elem.children('.move.button').on('mousedown', function(ev) {
        setTop(elem);
		drag.elem = elem;
		drag.prev = point(ev.clientX, ev.clientY);
    });
	
	elem.children('.grabber').on('mousedown', function(ev) {
		resize.elem = elem;
		resize.prev = ev.clientX;
    });
}

// Resizing
function resizeLabel(ev) {
    if (resize.elem == null) {
        return;
    }
    var scale = getScale();

    var curr = ev.clientX;
    var delta = (curr - resize.prev) / scale.X;
    resize.prev = curr;

    var w = resize.elem.width();
    setWidth(resize.elem, Math.max(100, Math.round(w + delta)));
}

function resizeEnd() {
    var elem = resize.elem;

	if (elem !== null) {
        aardvark.processEvent(elem.attr('id'), 'onlabelresized', elem.width());
    }
    
    resize.elem = null;
}

// Dragging
function dragLabel(ev) {
    var elem = drag.elem;

    if (elem == null) {
        return;
    }

    var scale = getScale();

    var curr = point(ev.clientX, ev.clientY);
    var delta = point((curr.X - drag.prev.X) / scale.X, (curr.Y - drag.prev.Y) / scale.Y);
    drag.prev = curr;

    // Drag the label
    var pos = elem.data('pos');
    setPosition(elem, point(Math.round(pos.X + delta.X),
                            Math.round(pos.Y + delta.Y)));
}

function dragEnd() {
    var elem = drag.elem;

	if (elem !== null) {
        aardvark.processEvent(elem.attr('id'), 'onlabelmoved', elem.data('pos'));
    }
    
    drag.elem = null;
}

// Register event listeners
document.addEventListener('mousemove', function(ev) {
	dragLabel(ev);
	resizeLabel(ev);
});

document.addEventListener('mouseup', function() {
	dragEnd();
	resizeEnd();
});

window.addEventListener('resize', function() {
    $('.annotations .label').each(function() {
        transform($(this)); 
    });
});