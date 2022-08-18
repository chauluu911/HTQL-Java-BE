package gui;

import org.jasypt.encryption.pbe.PooledPBEStringEncryptor;
import org.jasypt.exceptions.EncryptionOperationNotPossibleException;
import org.jasypt.iv.RandomIvGenerator;
import util.ConfigurationConstant;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class EncryptForm extends JDialog {
    private JPanel contentPane;
    private JButton btnCancel;
    private JButton btnEncrypt;
    private JButton btnDecrypt;
    private JTextField txtEncrypt;
    private JTextArea txtDecrypt;
    private JTextArea txtResultEncrypt;
    private JTextField txtResultDecrypt;
    private JLabel label1;
    private JLabel label2;
    private JTextField txtPassword1;
    private JTextField txtPassword2;
    private JLabel label11;
    private JLabel label22;

    private EncryptForm() {
        setContentPane(contentPane);
        setModal(true);
        setTitle(ConfigurationConstant.Dialog.TITLE);
        getRootPane().setDefaultButton(btnCancel);

        btnCancel.addActionListener(e -> onCancel());

        // call onCancel() when cross is clicked
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onCancel();
            }
        });

        // call onCancel() on ESCAPE
        contentPane.registerKeyboardAction(e -> onCancel(), KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
                JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);

        btnEncrypt.addActionListener(e -> onEncrypt());
        btnDecrypt.addActionListener(e -> onDecrypt());

        txtEncrypt.addKeyListener(new KeyAdapter() {
            /**
             * Invoked when a key has been pressed.
             *
             * @param e KeyEvent
             */
            public void keyPressed(KeyEvent e) {
                txtEncrypt.setText(txtEncrypt.getText().replaceAll("\\s+", ""));
            }
        });

        txtPassword1.addKeyListener(new KeyAdapter() {
            /**
             * Invoked when a key has been pressed.
             *
             * @param e KeyEvent
             */
            public void keyPressed(KeyEvent e) {
                txtPassword1.setText(txtPassword1.getText().replaceAll("\\s+", ""));
            }
        });

        txtPassword2.addKeyListener(new KeyAdapter() {
            /**
             * Invoked when a key has been pressed.
             *
             * @param e KeyEvent
             */
            public void keyPressed(KeyEvent e) {
                txtPassword2.setText(txtPassword2.getText().replaceAll("\\s+", ""));
            }
        });

        txtDecrypt.addKeyListener(new KeyAdapter() {
            /**
             * Invoked when a key has been pressed.
             *
             * @param e KeyEvent
             */
            public void keyPressed(KeyEvent e) {
                txtDecrypt.setText(txtDecrypt.getText().replaceAll("\\s+", ""));
            }
        });
    }

    public static void main(String[] args) {
        EncryptForm dialog = new EncryptForm();
        dialog.pack();

        // set dialog to center
        dialog.setLocation((Toolkit.getDefaultToolkit().getScreenSize().width) / 2 - dialog.getWidth() / 2,
                (Toolkit.getDefaultToolkit().getScreenSize().height) / 2 - dialog.getHeight() / 2);

        dialog.setVisible(true);
        System.exit(0);
    }

    private void onCancel() {
        // add your code here if necessary
        dispose();
    }

    /**
     * Encrypt
     */
    private void onEncrypt() {
        String strEn = txtEncrypt.getText();
        String password = txtPassword1.getText();

        if (strEn != null && !strEn.trim().equals("") && password != null && !password.trim().equals("")) {
            PooledPBEStringEncryptor encrypt = this.encryptFactory(password);

            txtResultEncrypt.setText(encrypt.encrypt(strEn));

            label1.setVisible(false);
            label11.setVisible(false);
        } else {
            if (strEn == null || strEn.trim().equals("")) {
                label1.setVisible(true);
            }
            if (password == null || password.trim().equals("")) {
                label11.setVisible(true);
            }
        }
    }

    /**
     * Decrypt
     */
    private void onDecrypt() {
        String strDe = txtDecrypt.getText();
        String password = txtPassword2.getText();
        if (strDe != null && !strDe.trim().equals("") && password != null && !password.trim().equals("")) {
            try {
                PooledPBEStringEncryptor decrypt = this.encryptFactory(password);
                txtResultDecrypt.setForeground(Color.BLACK);
                txtResultDecrypt.setText(decrypt.decrypt(strDe));
            } catch (EncryptionOperationNotPossibleException e) {
                txtResultDecrypt.setText("Incorrect input or password");
                txtResultDecrypt.setForeground(Color.RED);
            }
            label2.setVisible(false);
            label22.setVisible(false);
        } else {
            if (strDe == null || strDe.trim().equals("")) {
                label2.setVisible(true);
            }
            if (password == null || password.trim().equals("")) {
                label22.setVisible(true);
            }
        }
    }

    /**
     * encrypt factory
     *
     * @param password String
     * @return PooledPBEStringEncryptor
     */
    private PooledPBEStringEncryptor encryptFactory(String password) {
        PooledPBEStringEncryptor encrypts = new PooledPBEStringEncryptor();
        encrypts.setPoolSize(ConfigurationConstant.Jasypt.POOL_SIZE);
        encrypts.setAlgorithm(ConfigurationConstant.Jasypt.ALGORITHM);
        encrypts.setPassword(password);
        encrypts.setIvGenerator(new RandomIvGenerator());

        return encrypts;
    }
}
