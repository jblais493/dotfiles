/**
 * StatusAreaHorizontalSpacing extension
 * v2.1.5
 *
 * This extension essentially modifies the "-natural-hpadding"
 * attribute of panel-buttons (i.e. indicators in the status area)
 * so that they can be closer together.
 *
 * The default is 12.
 *
 * It does this by using 'set_style' to override the '-natural-hpadding'
 * property of anything added to Main.panel._rightBox.
 * It listens to the 'add-actor' signal of Main.panel._rightBox to override
 * the style.
 *
 * 2012 mathematical.coffee@gmail.com
 */

/****************************
 * CODE
 ****************************/
import * as Main from 'resource:///org/gnome/shell/ui/main.js';

import { Extension } from 'resource:///org/gnome/shell/extensions/extension.js';

export default class StatusAreaHorizontalSpacingExtension extends Extension {
    enable() {
        this.actorAddedID = null;
        this.hpaddingChangedID = null;
        this.styleLine = null;
        this.padding = null;
        this.settings = this.getSettings();

        this.applyStyles();

        /* whenever the settings get changed, re-layout everything. */
        this.hpaddingChangedID = this.settings.connect('changed::hpadding', () => {
            this.removeStyles();
            this.applyStyles();
        });
    }

    disable() {
        /*
        * This extension requires unlock-dialog session-mode because its functionality
        * is useful in lockscreen as well. The rationale is well explained in
        * https://gitlab.com/p91paul/status-area-horizontal-spacing-gnome-shell-extension/-/merge_requests/5
        * Preventing disabling when locking also works around a gnome shell crash.
        */
        this.removeStyles();
        if (this.hpaddingChangedID) {
            this.settings.disconnect(this.hpaddingChangedID);
        }
        this.settings = null;
    }

    /* Note: the gnome-shell class always overrides any you add in the extension.
    * So doing add_style_class(my_style_with_less_hpadding) doesn't work.
    * However set_style sets the inline style and that works.
    */
    _refreshActor(actor) {
        // thanks to https://github.com/home-sweet-gnome/dash-to-panel/commit/d372e6abd393b8f1c0e791b043dc2283b41d3ffb
        let oldClass = actor.get_style_class_name();
        actor.set_style_class_name('dummy-class-unlikely-to-exist-status-area-horizontal-spacing');
        actor.set_style_class_name(oldClass);
    }

    overrideStyle(actor, secondTime) {
        // it could be that the first child has the right style class name.
        if (!actor.has_style_class_name || !actor.has_style_class_name('panel-button')) {
            if (secondTime) {
                // if we've already recursed once, then give up (we will only look
                // one level down to find the 'panel-button' actor).
                return;
            }
            let child = actor.get_children();
            if (child.length) {
                this.overrideStyle(child[0], true);
            }
            return;
        }

        if (actor._original_inline_style_ === undefined) {
            actor._original_inline_style_ = actor.get_style();
        }
        actor.set_style(this.styleLine + '; ' + (actor._original_inline_style_ || ''));
        /* listen for the style being set externally so we can re-apply our style */
        // TODO: somehow throttle the number of calls to this - add a timeout with
        // a flag?
        if (!actor._statusAreaHorizontalSpacingSignalID) {
            actor._statusAreaHorizontalSpacingSignalID =
                actor.connect('style-changed', () => {
                    let currStyle = actor.get_style();
                    if (currStyle && !currStyle.match(this.styleLine)) {
                        // re-save the style (if it has in fact changed)
                        actor._original_inline_style_ = currStyle;
                        // have to do this or else the overrideStyle call will trigger
                        // another call of this, firing an endless series of these signals.
                        // TODO: a ._style_pending which prevents it rather than disconnect/connect?
                        actor.disconnect(actor._statusAreaHorizontalSpacingSignalID);
                        delete actor._statusAreaHorizontalSpacingSignalID;
                        this.overrideStyle(actor);
                    }
                });
        }
        this._refreshActor(actor);
    }

    // see the note in overrideStyle about us having to recurse down to the first
    // child of `actor` in order to find the container with style class name
    // 'panel-button' (applying our style to the parent container won't work).
    restoreOriginalStyle(actor, secondTime) {
        if (!actor.has_style_class_name || !actor.has_style_class_name('panel-button')) {
            if (secondTime) {
                // if we've already recursed once, then give up (we will only look
                // one level down to find the 'panel-button' actor).
                return;
            }
            let child = actor.get_children();
            if (child.length) {
                this.restoreOriginalStyle(child[0], true);
            }
            return;
        }
        if (actor._statusAreaHorizontalSpacingSignalID) {
            actor.disconnect(actor._statusAreaHorizontalSpacingSignalID);
            delete actor._statusAreaHorizontalSpacingSignalID;
        }
        if (actor._original_inline_style_ !== undefined) {
            actor.set_style(actor._original_inline_style_);
            delete actor._original_inline_style_;
        }
        this._refreshActor(actor);
    }

    /* Apply hpadding style to all existing actors & listen for more */
    applyStyles() {
        this.padding = this.settings.get_int('hpadding');
        this.styleLine = '-natural-hpadding: %dpx'.format(this.padding);
        // if you set it below 6 and it looks funny, that's your fault!
        if (this.padding < 6) {
            this.styleLine += '; -minimum-hpadding: %dpx'.format(this.padding);
        }

        /* set style for everything in _rightBox */
        let children = Main.panel._rightBox.get_children();
        for (let i = 0; i < children.length; ++i) {
            this.overrideStyle(children[i]);
        }

        /* connect signal */
        this.actorAddedID = Main.panel._rightBox.connect('child-added', (container, actor) => {
            this.overrideStyle(actor);
        });
    }

    /* Remove hpadding style from all existing actors & stop listening for more */
    removeStyles() {
        /* disconnect signal */
        if (this.actorAddedID) {
            Main.panel._rightBox.disconnect(this.actorAddedID);
        }

        /* remove style class name. */
        let children = Main.panel._rightBox.get_children();
        for (let i = 0; i < children.length; ++i) {
            this.restoreOriginalStyle(children[i]);
        }
    }
}
