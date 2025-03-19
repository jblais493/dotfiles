'use strict';
import Adw from 'gi://Adw';
import Gio from 'gi://Gio';
import GLib from 'gi://GLib';
import GObject from 'gi://GObject';

export const  Asus = GObject.registerClass({
    GTypeName: 'BHC_Asus',
    Template: GLib.Uri.resolve_relative(import.meta.url, '../ui/asus.ui', GLib.UriFlags.NONE),
    InternalChildren: [
        'skip_threshold_verification',
    ],
}, class Asus extends Adw.PreferencesPage {
    constructor(settings) {
        super({});
        settings.bind(
            'skip-threshold-verification',
            this._skip_threshold_verification,
            'active',
            Gio.SettingsBindFlags.DEFAULT
        );
    }
});
