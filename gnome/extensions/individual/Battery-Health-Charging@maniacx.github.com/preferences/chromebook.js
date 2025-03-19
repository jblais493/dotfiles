'use strict';
import Adw from 'gi://Adw';
import GLib from 'gi://GLib';
import Gtk from 'gi://Gtk';
import GObject from 'gi://GObject';
import {gettext as _} from 'resource:///org/gnome/Shell/Extensions/js/extensions/prefs.js';

export const Chromebook = GObject.registerClass({
    GTypeName: 'BHC_Chromebook',
    Template: GLib.Uri.resolve_relative(import.meta.url, '../ui/chromebook.ui', GLib.UriFlags.NONE),
    InternalChildren: [
        'device_settings_group',
        'choose_configuration',
    ],
}, class Chromebook extends Adw.PreferencesPage {
    constructor(settings) {
        super({});
        this._settings = settings;
        const configurationMapping = {
            'sysfs': _('Sysfs node'),
            'ectool': _('Ectool'),
        };

        const supportedConfigs = this._settings.get_strv('multiple-configuration-supported');
        const displayNames = supportedConfigs.map(config => configurationMapping[config]);
        const stringList = new Gtk.StringList();
        displayNames.forEach(name => stringList.append(name));
        this._choose_configuration.set_model(stringList);
        const currentConfigMode = this._settings.get_string('configuration-mode');
        const initialSelectedIndex = supportedConfigs.indexOf(currentConfigMode);
        if (initialSelectedIndex !== -1)
            this._choose_configuration.set_selected(initialSelectedIndex);

        this._choose_configuration.connect('notify::selected-item', () => {
            const selectedIndex = this._choose_configuration.get_selected();
            const selectedConfig = supportedConfigs[selectedIndex];
            this._settings.set_string('configuration-mode', selectedConfig);
        });
    }
});
