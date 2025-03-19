import Clutter from "gi://Clutter";
import GObject from "gi://GObject";
import Pango from "gi://Pango";
import St from "gi://St";
const SCROLL_ANIMATION_SPEED = 0.04;
class ScrollingLabel extends St.ScrollView {
    label;
    box;
    onAdjustmentChangedId;
    onShowChangedId;
    isScrolling;
    isFixedWidth;
    initPaused;
    labelWidth;
    direction;
    transition;
    constructor(params) {
        super({
            hscrollbarPolicy: St.PolicyType.NEVER,
            vscrollbarPolicy: St.PolicyType.NEVER,
        });

        const defaultParams = {
            direction: Clutter.TimelineDirection.FORWARD,
            isFixedWidth: true,
        };
        const { text, width, direction, isFixedWidth, isScrolling, initPaused } = {
            ...defaultParams,
            ...params,
        };
        this.isScrolling = isScrolling;
        this.isFixedWidth = isFixedWidth;
        this.initPaused = initPaused;
        this.labelWidth = width;
        this.direction = direction;

        this.box = new St.BoxLayout({
            xExpand: true,
            yExpand: true,
        });

        this.label = new St.Label({
            text,
            yAlign: Clutter.ActorAlign.CENTER,
            xAlign: Clutter.ActorAlign.START,
        });

        this.onShowChangedId = this.label.connect("show", this.onShowChanged.bind(this));
        this.box.add_child(this.label);

        if (Clutter.Container === undefined) {
            this.add_child(this.box);
        }
        else {
            this.add_actor(this.box);
        }
    }
    pauseScrolling() {
        this.transition?.pause();
        this.initPaused = true;
    }
    resumeScrolling() {
        this.transition?.start();
        this.initPaused = false;
    }
    initScrolling() {
        const adjustment = this.hscroll.adjustment;
        const origText = this.label.text;
        this.onAdjustmentChangedId = adjustment.connect("changed", this.onAdjustmentChanged.bind(this, adjustment, origText));
        this.label.text = `${origText} `;
        this.label.clutterText.ellipsize = Pango.EllipsizeMode.NONE;
    }
    onAdjustmentChanged(adjustment, origText) {
        if (adjustment.upper <= adjustment.pageSize) {
            return;
        }

        const initial = adjustment.value;
        const final = adjustment.upper;
        const duration = adjustment.upper / SCROLL_ANIMATION_SPEED;
        const pspec = adjustment.find_property("value");
        const interval = new Clutter.Interval({
            valueType: pspec.value_type,
            initial,
            final,
        });

        this.transition = new Clutter.PropertyTransition({
            propertyName: "value",
            progressMode: Clutter.AnimationMode.LINEAR,
            direction: this.direction,
            repeatCount: -1,
            duration,
            interval,
        });

        this.label.text = `${origText} ${origText}`;
        adjustment.add_transition("scroll", this.transition);
        adjustment.disconnect(this.onAdjustmentChangedId);

        if (this.initPaused) {
            this.transition.pause();
        }
    }
    onShowChanged() {
        if (this.label.visible === false) {
            return;
        }

        const isLabelWider = this.label.width > this.labelWidth && this.labelWidth > 0;

        if (isLabelWider && this.isScrolling) {
            this.initScrolling();
        }

        if (this.isFixedWidth && this.labelWidth > 0) {
            this.box.width = this.labelWidth;
            this.label.xAlign = Clutter.ActorAlign.CENTER;
            this.label.xExpand = true;
        }
        else if (isLabelWider) {
            this.box.width = Math.min(this.label.width, this.labelWidth);
        }

        this.label.disconnect(this.onShowChangedId);
    }
    vfunc_scroll_event() {
        return Clutter.EVENT_PROPAGATE;
    }
}
const GScrollingLabel = GObject.registerClass({
    GTypeName: "ScrollingLabel",
}, ScrollingLabel);
export default GScrollingLabel;
